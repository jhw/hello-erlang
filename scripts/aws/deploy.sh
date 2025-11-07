#!/bin/bash
# Application deployment to EC2

set -e

cd "$(dirname "$0")/../.."

# Load local AWS config if it exists (gitignored)
if [ -f "config/aws.sh" ]; then
    source "config/aws.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
DEPLOY_DIR="${DEPLOY_DIR:-/opt/hello_erlang}"
RELEASE_NAME="hello_erlang"

usage() {
    echo "Usage: $0 <environment> [options]"
    echo ""
    echo "Deploy hello_erlang application to EC2"
    echo ""
    echo "Arguments:"
    echo "  <environment>      - Environment to deploy to (dev, staging, prod)"
    echo ""
    echo "Options:"
    echo "  --skip-build       - Skip building the tarball (use existing)"
    echo "  --key-file <path>  - SSH key file path (default: ~/.ssh/<stack-key>.pem)"
    echo ""
    echo "Examples:"
    echo "  $0 dev"
    echo "  $0 prod --key-file ~/.ssh/my-key.pem"
    exit 1
}

get_stack_output() {
    local env=$1
    local output_key=$2
    local stack_name="${STACK_PREFIX}-${env}"

    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query "Stacks[0].Outputs[?OutputKey=='$output_key'].OutputValue" \
        --output text
}

get_instance_ip() {
    local env=$1
    get_stack_output "$env" "PublicIP"
}

build_tarball() {
    echo "Building release tarball..."
    rebar3 tar

    # Find the tarball
    TARBALL=$(find _build/default/rel -name "*.tar.gz" | head -1)

    if [ -z "$TARBALL" ]; then
        echo "Error: Tarball not found after build"
        exit 1
    fi

    echo "✓ Tarball built: $TARBALL"
}

upload_tarball() {
    local instance_ip=$1
    local key_file=$2

    echo ""
    echo "Uploading tarball to $instance_ip..."

    scp -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "$TARBALL" \
        "ec2-user@${instance_ip}:${DEPLOY_DIR}/"

    echo "✓ Tarball uploaded"
}

deploy_release() {
    local instance_ip=$1
    local key_file=$2

    echo ""
    echo "Deploying release on $instance_ip..."

    local tarball_name=$(basename "$TARBALL")

    # Deploy script to run on remote server
    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<EOF
set -e

cd ${DEPLOY_DIR}

echo "Stopping existing release (if running)..."
if [ -f ./bin/${RELEASE_NAME} ]; then
    ./bin/${RELEASE_NAME} stop || true
    sleep 2
fi

echo "Backing up current release..."
if [ -d "./bin" ]; then
    rm -rf ./backup
    mkdir -p ./backup
    mv bin lib releases ./backup/ 2>/dev/null || true
fi

echo "Extracting new release..."
tar -xzf ${tarball_name}

echo "Starting new release..."
./bin/${RELEASE_NAME} daemon

sleep 2

echo "Checking release status..."
if ./bin/${RELEASE_NAME} pid > /dev/null 2>&1; then
    PID=\$(./bin/${RELEASE_NAME} pid)
    echo "✓ Release started successfully (PID: \$PID)"
else
    echo "✗ Failed to start release"
    exit 1
fi

echo "Cleaning up..."
rm -f ${tarball_name}

echo ""
echo "Deployment complete!"
EOF

    echo "✓ Deployment complete"
}

verify_deployment() {
    local instance_ip=$1

    echo ""
    echo "Verifying deployment..."

    sleep 2

    local response=$(curl -s "http://${instance_ip}:8080/echo?message=Deployment+Test" || echo "")

    if [ "$response" == "Deployment Test" ]; then
        echo "✓ Application responding correctly!"
        echo ""
        echo "Test URL: http://${instance_ip}:8080/echo?message=Hello"
    else
        echo "✗ Application not responding as expected"
        echo "Response: $response"
        exit 1
    fi
}

# Main deployment flow
deploy() {
    local env=$1
    local skip_build=false
    local key_file=""

    # Parse options
    shift
    while [[ $# -gt 0 ]]; do
        case $1 in
            --skip-build)
                skip_build=true
                shift
                ;;
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
    echo "  Deploying Hello Erlang to $env"
    echo "═══════════════════════════════════════════════"
    echo ""

    # Get instance IP from CloudFormation
    local instance_ip=$(get_instance_ip "$env")

    if [ -z "$instance_ip" ]; then
        echo "Error: Could not get instance IP for environment '$env'"
        echo "Make sure the stack exists: ./scripts/aws/stack.sh status $env"
        exit 1
    fi

    echo "Target: $instance_ip"
    echo ""

    # Determine SSH key file
    if [ -z "$key_file" ]; then
        # Try to get key name from stack parameters
        local stack_name="${STACK_PREFIX}-${env}"
        local key_name=$(aws cloudformation describe-stacks \
            --stack-name "$stack_name" \
            --query "Stacks[0].Parameters[?ParameterKey=='KeyName'].ParameterValue" \
            --output text)

        if [ -z "$key_name" ]; then
            echo "Error: Could not determine SSH key name"
            echo "Please specify with --key-file option"
            exit 1
        fi

        key_file="$HOME/.ssh/${key_name}.pem"
    fi

    if [ ! -f "$key_file" ]; then
        echo "Error: SSH key file not found: $key_file"
        echo "Please specify correct key file with --key-file option"
        exit 1
    fi

    echo "SSH Key: $key_file"
    echo ""

    # Build tarball (unless skipped)
    if [ "$skip_build" = false ]; then
        build_tarball
    else
        echo "Skipping build (using existing tarball)..."
        TARBALL=$(find _build/default/rel -name "*.tar.gz" | head -1)
        if [ -z "$TARBALL" ]; then
            echo "Error: No existing tarball found"
            exit 1
        fi
        echo "Using: $TARBALL"
    fi

    # Upload and deploy
    upload_tarball "$instance_ip" "$key_file"
    deploy_release "$instance_ip" "$key_file"
    verify_deployment "$instance_ip"

    echo ""
    echo "═══════════════════════════════════════════════"
    echo "  ✓ Deployment Successful!"
    echo "═══════════════════════════════════════════════"
    echo ""
    echo "Application URL: http://${instance_ip}:8080/echo?message=Hello"
    echo "SSH Access: ssh -i $key_file ec2-user@${instance_ip}"
    echo ""
}

# Check arguments
if [ -z "$1" ]; then
    usage
fi

deploy "$@"
