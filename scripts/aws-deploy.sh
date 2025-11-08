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
    echo "Usage: $0 {build|start|stop|restart|status|ping|logs|init-status} <environment> [options]"
    echo ""
    echo "Commands:"
    echo "  init-status <env> [--follow] - Check if UserData initialization is complete"
    echo "  build <env>     - Build release in CodeBuild and deploy to EC2 instance"
    echo "  start <env>     - Start the application on EC2"
    echo "  stop <env>      - Stop the application on EC2"
    echo "  restart <env>   - Restart the application on EC2"
    echo "  status <env>    - Check application status on EC2"
    echo "  ping <env> [msg] - Test application endpoint (default message: 'ping')"
    echo "  logs <env>      - Tail initialization logs (useful during first boot)"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options:"
    echo "  --key-file <path>  - SSH key file path (default: auto-discover from stack)"
    echo ""
    echo "Deployment workflow:"
    echo "  0. $0 init-status dev  # Wait for UserData to complete"
    echo "  1. $0 build dev        # Build in CodeBuild and deploy to EC2"
    echo "  2. $0 start dev        # Start application"
    echo "  3. $0 ping dev         # Verify it's responding"
    echo ""
    echo "Other examples:"
    echo "  $0 logs dev        - Monitor UserData initialization"
    echo "  $0 status dev      - Check if app is running"
    echo "  $0 restart dev     - Restart running app"
    echo "  $0 stop dev        - Stop app"
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

    # Get stack outputs
    local codebuild_project=$(get_stack_output "$env" "CodeBuildProjectName")
    local artifact_bucket=$(get_stack_output "$env" "ArtifactBucketName")

    if [ -z "$codebuild_project" ]; then
        echo "Error: Could not get CodeBuild project name from stack outputs"
        exit 1
    fi

    if [ -z "$artifact_bucket" ]; then
        echo "Error: Could not get artifact bucket name from stack outputs"
        exit 1
    fi

    echo "=== CodeBuild Deployment ==="
    echo "Building release in CodeBuild..."
    echo "  Project: $codebuild_project"
    echo "  Artifact Bucket: $artifact_bucket"
    echo ""

    # Create source bundle
    echo "Creating source bundle..."
    local timestamp=$(date +%Y%m%d-%H%M%S)
    local source_bundle="/tmp/hello_erlang_source_${timestamp}.zip"

    zip -q -r "$source_bundle" \
        apps config/buildspec.yml rebar.config rebar.lock \
        -x "*.git*" "*_build*" "*.rebar3*" "*.beam" "config/ec2-stack.yaml" "config/aws.sh"

    echo "✓ Source bundle created: $(basename $source_bundle)"
    echo ""

    # Upload source to S3
    local source_key="sources/source-${timestamp}.zip"
    echo "Uploading source to S3..."
    aws s3 cp "$source_bundle" "s3://${artifact_bucket}/${source_key}"
    echo "✓ Source uploaded to s3://${artifact_bucket}/${source_key}"
    echo ""

    # Start CodeBuild
    echo "Starting CodeBuild..."
    local build_id=$(aws codebuild start-build \
        --project-name "$codebuild_project" \
        --source-type-override S3 \
        --source-location-override "${artifact_bucket}/${source_key}" \
        --query 'build.id' \
        --output text)

    if [ -z "$build_id" ]; then
        echo "Error: Failed to start CodeBuild"
        rm -f "$source_bundle"
        exit 1
    fi

    echo "✓ Build started: $build_id"
    echo ""

    # Get log group and stream info
    local log_group="/aws/codebuild/${codebuild_project}"
    local log_stream="${build_id#*:}"

    echo "Tailing CodeBuild logs (this may take a moment to start)..."
    echo "---"
    echo ""

    # Wait for build to complete while tailing logs
    local status="IN_PROGRESS"
    local next_token=""
    local last_check=0

    while [ "$status" == "IN_PROGRESS" ]; do
        # Check build status every 5 seconds
        local now=$(date +%s)
        if [ $((now - last_check)) -ge 5 ]; then
            status=$(aws codebuild batch-get-builds --ids "$build_id" --query 'builds[0].buildStatus' --output text 2>/dev/null || echo "IN_PROGRESS")
            last_check=$now
        fi

        # Try to fetch and display logs
        local log_output
        if [ -z "$next_token" ]; then
            log_output=$(aws logs get-log-events \
                --log-group-name "$log_group" \
                --log-stream-name "$log_stream" \
                --start-from-head \
                --output json 2>/dev/null || echo '{}')
        else
            log_output=$(aws logs get-log-events \
                --log-group-name "$log_group" \
                --log-stream-name "$log_stream" \
                --next-token "$next_token" \
                --start-from-head \
                --output json 2>/dev/null || echo '{}')
        fi

        # Display log events
        echo "$log_output" | jq -r '.events[]?.message' 2>/dev/null

        # Get next token for pagination
        local new_token=$(echo "$log_output" | jq -r '.nextForwardToken // empty' 2>/dev/null)
        if [ -n "$new_token" ] && [ "$new_token" != "$next_token" ]; then
            next_token="$new_token"
        fi

        sleep 2
    done

    # Fetch any remaining logs
    if [ -n "$next_token" ]; then
        aws logs get-log-events \
            --log-group-name "$log_group" \
            --log-stream-name "$log_stream" \
            --next-token "$next_token" \
            --start-from-head \
            --output json 2>/dev/null | jq -r '.events[]?.message' 2>/dev/null || true
    fi

    echo ""
    echo "---"

    # Clean up source bundle
    rm -f "$source_bundle"

    echo ""
    if [ "$status" == "SUCCEEDED" ]; then
        echo "✓ CodeBuild completed successfully"
        echo ""

        # Find the built artifact in S3
        echo "Finding built release artifact..."
        local artifact_prefix="releases/${build_id#*:}"
        local artifact_key=$(aws s3 ls "s3://${artifact_bucket}/${artifact_prefix}/" --recursive | \
            grep "\.tar\.gz$" | awk '{print $4}' | head -1)

        if [ -z "$artifact_key" ]; then
            echo "Error: Could not find release artifact in S3"
            exit 1
        fi

        local tarball_name=$(basename "$artifact_key")
        echo "✓ Found artifact: $tarball_name"
        echo ""

        # Download artifact to EC2
        echo "Deploying release to EC2 instance $instance_ip..."
        ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            "ec2-user@${instance_ip}" bash <<EOF
set -e

cd ${DEPLOY_DIR}

echo "Downloading release from S3..."
aws s3 cp "s3://${artifact_bucket}/${artifact_key}" "${tarball_name}"

echo "Stopping existing release (if running)..."
if [ -f ./bin/${RELEASE_NAME} ]; then
    ./bin/${RELEASE_NAME} stop || true
    sleep 2
fi

echo "Backing up current release..."
if [ -d "./bin" ]; then
    rm -rf ./backup
    mkdir -p ./backup
    mv bin lib releases erts-* ./backup/ 2>/dev/null || true
fi

echo "Extracting release..."
tar -xzf "${tarball_name}"

echo "✓ Release deployed successfully"
ls -lh ${tarball_name}
EOF

        echo ""
        echo "✓ Deployment complete!"
        echo ""
        echo "Next step: $0 start $env"
    else
        echo "✗ CodeBuild failed with status: $status"
        echo ""
        echo "View logs with:"
        echo "  aws codebuild batch-get-builds --ids $build_id"
        exit 1
    fi
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

if [ ! -f ./bin/hello_erlang ]; then
    echo "Error: Release not found. Run upload first."
    exit 1
fi

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

    # Get ALB DNS name from stack outputs
    local alb_dns=$(get_stack_output "$env" "LoadBalancerDNS")
    if [ -z "$alb_dns" ]; then
        echo "Error: Could not get Load Balancer DNS for environment '$env'" >&2
        exit 1
    fi

    # URL encode the message
    local encoded_message=$(printf %s "$message" | jq -sRr @uri)

    local url="http://${alb_dns}/echo?message=${encoded_message}"

    echo "Testing application endpoint..."
    echo "  URL: $url"
    echo ""

    if ! command -v curl &> /dev/null; then
        echo "Error: curl not found. Please install curl."
        exit 1
    fi

    local response=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$url" 2>/dev/null)
    local http_code=$(echo "$response" | tail -n 1)
    local body=$(echo "$response" | sed '$d')

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

cmd_logs() {
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

    echo "Tailing initialization logs from $instance_ip..."
    echo "Press Ctrl+C to exit"
    echo ""

    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" \
        "tail -f /var/log/hello-erlang-init.log"
}

cmd_init_status() {
    local env=$1
    shift
    local key_file=""
    local follow=false

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            --follow|-f)
                follow=true
                shift
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    echo "Checking initialization status on $instance_ip..."
    echo ""

    # Check if deployment directory exists
    if ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        -o ConnectTimeout=5 \
        "ec2-user@${instance_ip}" \
        "test -d ${DEPLOY_DIR}" 2>/dev/null; then

        echo "✓ Deployment directory exists: ${DEPLOY_DIR}"

        # Check if Erlang is installed
        if ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            "ec2-user@${instance_ip}" \
            "test -f /usr/local/erlang/bin/erl" 2>/dev/null; then

            local erl_version=$(ssh -i "$key_file" \
                -o StrictHostKeyChecking=no \
                -o UserKnownHostsFile=/dev/null \
                "ec2-user@${instance_ip}" \
                "/usr/local/erlang/bin/erl -version 2>&1")

            echo "✓ Erlang installed: $erl_version"
        else
            echo "✗ Erlang not yet installed"
        fi

        # Check if rebar3 is installed
        if ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            "ec2-user@${instance_ip}" \
            "command -v rebar3 > /dev/null" 2>/dev/null; then

            echo "✓ rebar3 installed"
        else
            echo "✗ rebar3 not yet installed"
        fi

        # Check if init log exists
        if ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            "ec2-user@${instance_ip}" \
            "test -f /var/log/hello-erlang-init.log" 2>/dev/null; then

            echo "✓ Initialization complete"
            echo ""
            ssh -i "$key_file" \
                -o StrictHostKeyChecking=no \
                -o UserKnownHostsFile=/dev/null \
                "ec2-user@${instance_ip}" \
                "cat /var/log/hello-erlang-init.log"
        else
            echo "⏳ Initialization still in progress"
            echo ""
            if [ "$follow" = true ]; then
                echo "Following cloud-init logs (Press Ctrl+C to exit)..."
                echo ""
                ssh -i "$key_file" \
                    -o StrictHostKeyChecking=no \
                    -o UserKnownHostsFile=/dev/null \
                    "ec2-user@${instance_ip}" \
                    "sudo tail -f /var/log/cloud-init-output.log"
            else
                echo "Run './scripts/aws-deploy.sh init-status $env --follow' to monitor progress"
            fi
        fi
    else
        echo "✗ Deployment directory not found"
        echo "⏳ UserData script still running (compiling Erlang/OTP)"
        echo ""
        if [ "$follow" = true ]; then
            echo "Following cloud-init logs (Press Ctrl+C to exit)..."
            echo ""
            ssh -i "$key_file" \
                -o StrictHostKeyChecking=no \
                -o UserKnownHostsFile=/dev/null \
                "ec2-user@${instance_ip}" \
                "sudo tail -f /var/log/cloud-init-output.log"
        else
            echo "This can take 5-10 minutes. Run with --follow to monitor:"
            echo "  ./scripts/aws-deploy.sh init-status $env --follow"
        fi
    fi
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
        cmd_build "$ENV" "$@"
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
    logs)
        cmd_logs "$ENV" "$@"
        ;;
    init-status)
        cmd_init_status "$ENV" "$@"
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac
