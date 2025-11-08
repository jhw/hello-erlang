#!/bin/bash
# Application deployment to EC2

set -e

cd "$(dirname "$0")/.."

# Load local AWS config if it exists (gitignored)
if [ -f "config/aws.sh" ]; then
    source "config/aws.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
DEPLOY_DIR="${DEPLOY_DIR:-/opt/hello_erlang}"
RELEASE_NAME="hello_erlang"

usage() {
    echo "Usage: $0 {build|clean|upload|start|stop|restart|status|ping|deploy} <environment> [options]"
    echo ""
    echo "Commands:"
    echo "  upload <env>    - Upload source code to EC2 instance"
    echo "  build <env>     - Build release on EC2 server"
    echo "  clean <env>     - Remove local tarball (deprecated)"
    echo "  start <env>     - Start the application on EC2"
    echo "  stop <env>      - Stop the application on EC2"
    echo "  restart <env>   - Restart the application on EC2"
    echo "  status <env>    - Check application status on EC2"
    echo "  ping <env> [msg] - Test application endpoint (default message: 'ping')"
    echo "  deploy <env>    - Full deployment (build + upload + start)"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options:"
    echo "  --key-file <path>  - SSH key file path (default: auto-discover from stack)"
    echo ""
    echo "Examples:"
    echo "  $0 upload dev             - Upload source code to dev"
    echo "  $0 build dev              - Build release on dev server"
    echo "  $0 start dev              - Start application"
    echo "  $0 ping dev               - Test application endpoint"
    echo "  $0 ping dev Hello         - Test with custom message"
    echo "  $0 restart dev            - Restart app in dev environment"
    echo "  $0 deploy prod            - Full deploy: upload + build + start"
    exit 1
}

# Helper functions
get_stack_output() {
    local env=$1
    local output_key=$2
    local stack_name="${STACK_PREFIX}-${env}"

    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query "Stacks[0].Outputs[?OutputKey=='$output_key'].OutputValue" \
        --output text 2>/dev/null
}

get_instance_ip() {
    local env=$1
    local ip=$(get_stack_output "$env" "PublicIP")

    if [ -z "$ip" ]; then
        echo "Error: Could not get instance IP for environment '$env'" >&2
        echo "Make sure the stack exists: ./scripts/aws-stack.sh status $env" >&2
        return 1
    fi

    echo "$ip"
}

get_key_file() {
    local env=$1
    local key_file="$2"

    if [ -z "$key_file" ]; then
        local stack_name="${STACK_PREFIX}-${env}"
        local key_name=$(aws cloudformation describe-stacks \
            --stack-name "$stack_name" \
            --query "Stacks[0].Parameters[?ParameterKey=='KeyName'].ParameterValue" \
            --output text 2>/dev/null)

        if [ -z "$key_name" ] || [ "$key_name" == "None" ]; then
            echo "Error: No SSH key configured for this stack" >&2
            echo "Use SSM Session Manager or specify --key-file" >&2
            return 1
        fi

        key_file="$HOME/.ssh/${key_name}.pem"
    fi

    if [ ! -f "$key_file" ]; then
        echo "Error: SSH key file not found: $key_file" >&2
        return 1
    fi

    echo "$key_file"
}

check_tarball_exists() {
    local tarball=$(find _build/prod/rel -name "*.tar.gz" 2>/dev/null | head -1)
    if [ -z "$tarball" ]; then
        return 1
    fi
    return 0
}

get_tarball_path() {
    local tarball=$(find _build/prod/rel -name "*.tar.gz" 2>/dev/null | head -1)
    echo "$tarball"
}

check_tarball_on_remote() {
    local instance_ip=$1
    local key_file=$2
    local tarball_name=$(basename "$(get_tarball_path)")

    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        -o ConnectTimeout=5 \
        "ec2-user@${instance_ip}" \
        "test -f ${DEPLOY_DIR}/${tarball_name}" 2>/dev/null
}

check_app_extracted() {
    local instance_ip=$1
    local key_file=$2

    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        -o ConnectTimeout=5 \
        "ec2-user@${instance_ip}" \
        "test -f ${DEPLOY_DIR}/bin/${RELEASE_NAME}" 2>/dev/null
}

# Command implementations
cmd_build() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    echo "Building release on EC2 instance $instance_ip..."
    echo ""

    # Build on remote server
    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<'EOF'
set -e

cd /opt/hello_erlang

echo "Building release tarball with ERTS (prod mode)..."
export PATH=/usr/local/erlang/bin:$PATH
rebar3 as prod tar

# Find and show the tarball
TARBALL=$(find _build/prod/rel -name "*.tar.gz" 2>/dev/null | head -1)
if [ -z "$TARBALL" ]; then
    echo "Error: Tarball not found after build"
    exit 1
fi

echo "✓ Release built: $TARBALL"
EOF

    echo ""
    echo "✓ Build complete on server"
}

cmd_clean() {
    local env=$1

    if ! check_tarball_exists; then
        echo "No tarball found to clean"
        return 0
    fi

    local tarball=$(get_tarball_path)
    echo "Removing tarball: $tarball"
    rm -f "$tarball"
    echo "✓ Tarball removed"
}

cmd_upload() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    # Safety check: is app currently running?
    if check_app_extracted "$instance_ip" "$key_file"; then
        echo "Checking if application is running..."
        if ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            -o ConnectTimeout=5 \
            "ec2-user@${instance_ip}" \
            "cd ${DEPLOY_DIR} && ./bin/${RELEASE_NAME} pid > /dev/null 2>&1"; then

            echo ""
            echo "Error: Application is currently running"
            echo "Please stop the application before uploading:"
            echo "  $0 stop $env"
            exit 1
        fi
    fi

    echo "Uploading source code to $instance_ip..."
    echo "  Target: ${DEPLOY_DIR}/"
    echo "  Key: $key_file"
    echo ""

    # Create a tarball of source code (excluding build artifacts)
    tar -czf /tmp/hello_erlang_src.tar.gz \
        --exclude='.git' \
        --exclude='_build' \
        --exclude='.rebar3' \
        --exclude='*.beam' \
        -C "$(pwd)" \
        src config rebar.config rebar.lock

    # Upload source tarball
    scp -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        /tmp/hello_erlang_src.tar.gz \
        "ec2-user@${instance_ip}:/tmp/"

    # Extract source on server
    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<EOF
set -e
cd ${DEPLOY_DIR}
tar -xzf /tmp/hello_erlang_src.tar.gz
rm -f /tmp/hello_erlang_src.tar.gz
echo "✓ Source code extracted"
EOF

    # Clean up local temp file
    rm -f /tmp/hello_erlang_src.tar.gz

    echo ""
    echo "✓ Source code uploaded successfully"
    echo ""
    echo "Next step: $0 build $env"
}

cmd_start() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    echo "Starting application on $instance_ip..."
    echo ""

    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<'EOF'
set -e

cd /opt/hello_erlang

# Find the built tarball
TARBALL=$(find _build/prod/rel -name "*.tar.gz" 2>/dev/null | head -1)
if [ -z "$TARBALL" ]; then
    echo "Error: No release tarball found. Run build first."
    exit 1
fi

TARBALL_NAME=$(basename "$TARBALL")

echo "Stopping existing release (if running)..."
if [ -f ./bin/hello_erlang ]; then
    ./bin/hello_erlang stop || true
    sleep 2
fi

echo "Backing up current release..."
if [ -d "./bin" ]; then
    rm -rf ./backup
    mkdir -p ./backup
    mv bin lib releases erts-* ./backup/ 2>/dev/null || true
fi

echo "Extracting release from $TARBALL..."
tar -xzf "$TARBALL"

echo "Starting release..."
./bin/hello_erlang daemon

sleep 2

echo "Checking release status..."
if ./bin/hello_erlang pid > /dev/null 2>&1; then
    PID=$(./bin/hello_erlang pid)
    echo "✓ Release started successfully (PID: $PID)"
else
    echo "✗ Failed to start release"
    exit 1
fi
EOF

    echo ""
    echo "✓ Application started successfully"
    echo "  URL: http://${instance_ip}:8080/echo?message=Hello"
}

cmd_stop() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    # Check if app is extracted
    if ! check_app_extracted "$instance_ip" "$key_file"; then
        echo "Error: Application not found on remote server"
        exit 1
    fi

    echo "Stopping application on $instance_ip..."

    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<EOF
cd ${DEPLOY_DIR}
if [ -f ./bin/${RELEASE_NAME} ]; then
    ./bin/${RELEASE_NAME} stop || true
    echo "✓ Application stopped"
else
    echo "Application not found"
    exit 1
fi
EOF
}

cmd_restart() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    echo "Restarting application..."
    echo ""

    cmd_stop "$env" ${key_file:+--key-file "$key_file"}
    echo ""
    sleep 1
    cmd_start "$env" ${key_file:+--key-file "$key_file"}
}

cmd_status() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    echo "Checking application status on $instance_ip..."
    echo ""

    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<EOF
cd ${DEPLOY_DIR}

if [ ! -f ./bin/${RELEASE_NAME} ]; then
    echo "Status: NOT DEPLOYED"
    exit 0
fi

if ./bin/${RELEASE_NAME} pid > /dev/null 2>&1; then
    PID=\$(./bin/${RELEASE_NAME} pid)
    echo "Status: RUNNING (PID: \$PID)"

    # Try to check the application endpoint
    if command -v curl > /dev/null 2>&1; then
        RESPONSE=\$(curl -s http://localhost:8080/echo?message=test 2>/dev/null || echo "")
        if [ "\$RESPONSE" == "test" ]; then
            echo "Health: HEALTHY (responding to requests)"
        else
            echo "Health: DEGRADED (not responding correctly)"
        fi
    fi
else
    echo "Status: STOPPED"
fi
EOF

    echo ""
    echo "Application URL: http://${instance_ip}:8080/echo?message=Hello"
}

cmd_ping() {
    local env=$1
    shift
    local message="${1:-ping}"

    local instance_ip=$(get_instance_ip "$env") || exit 1
    local url="http://${instance_ip}:8080/echo?message=${message}"

    echo "Testing application endpoint..."
    echo "  URL: $url"
    echo ""

    if ! command -v curl &> /dev/null; then
        echo "Error: curl not found. Please install curl."
        exit 1
    fi

    local response=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$url" 2>/dev/null)
    local http_code=$(echo "$response" | tail -n1)
    local body=$(echo "$response" | head -n-1)

    if [ "$http_code" == "200" ] && [ "$body" == "$message" ]; then
        echo "✓ Application is responding"
        echo "  Status: 200 OK"
        echo "  Response: $body"
    elif [ "$http_code" == "200" ]; then
        echo "⚠ Application responded but with unexpected content"
        echo "  Status: 200 OK"
        echo "  Response: $body"
        echo "  Expected: $message"
    elif [ -n "$http_code" ]; then
        echo "✗ Application returned error"
        echo "  Status: $http_code"
        echo "  Response: $body"
        exit 1
    else
        echo "✗ Application not responding"
        echo "  Could not connect to $url"
        exit 1
    fi
}

cmd_deploy() {
    local env=$1
    shift
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    echo "═══════════════════════════════════════════════"
    echo "  Full Deployment to $env"
    echo "═══════════════════════════════════════════════"
    echo ""

    echo "[1/4] Uploading source code..."
    cmd_upload "$env" ${key_file:+--key-file "$key_file"}
    echo ""

    echo "[2/4] Building release on server..."
    cmd_build "$env" ${key_file:+--key-file "$key_file"}
    echo ""

    echo "[3/4] Starting application..."
    cmd_start "$env" ${key_file:+--key-file "$key_file"}
    echo ""

    echo "[4/4] Verifying deployment..."
    sleep 2
    cmd_ping "$env" "deployment-test"
    echo ""

    echo "═══════════════════════════════════════════════"
    echo "  ✓ Deployment Complete!"
    echo "═══════════════════════════════════════════════"
}

# Main command router
if [ -z "$1" ] || [ -z "$2" ]; then
    usage
fi

COMMAND=$1
ENV=$2
shift 2

case "$COMMAND" in
    build)
        cmd_build "$ENV"
        ;;
    clean)
        cmd_clean "$ENV"
        ;;
    upload)
        cmd_upload "$ENV" "$@"
        ;;
    start)
        cmd_start "$ENV" "$@"
        ;;
    stop)
        cmd_stop "$ENV" "$@"
        ;;
    restart)
        cmd_restart "$ENV" "$@"
        ;;
    status)
        cmd_status "$ENV" "$@"
        ;;
    ping)
        cmd_ping "$ENV" "$@"
        ;;
    deploy)
        cmd_deploy "$ENV" "$@"
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac
