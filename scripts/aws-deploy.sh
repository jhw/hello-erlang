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
    echo "Usage: $0 {build|deploy|start|stop|restart|status|ping|list-builds|list-artifacts|logs|init-status} <environment> [options]"
    echo ""
    echo "Build & Deploy Commands:"
    echo "  build <env>              - Build release in CodeBuild (outputs to S3)"
    echo "  deploy <env> [build]     - Deploy release from S3 to EC2 (latest or specific build-id)"
    echo "  list-builds <env>        - List recent CodeBuild builds"
    echo "  list-artifacts <env>     - List available releases in S3"
    echo "  logs <env> <build-id>    - Show logs for a CodeBuild build"
    echo ""
    echo "Application Commands:"
    echo "  start <env>              - Start the application on EC2"
    echo "  stop <env>               - Stop the application on EC2"
    echo "  restart <env>            - Restart the application on EC2"
    echo "  status <env>             - Check application status on EC2"
    echo "  ping <env> [msg]         - Test application endpoint (default message: 'ping')"
    echo "  init-status <env>        - Check if EC2 instance initialization is complete"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options:"
    echo "  --key-file <path>  - SSH key file path (default: auto-discover from stack)"
    echo ""
    echo "Deployment workflow:"
    echo "  1. $0 build dev             # Build in CodeBuild → S3"
    echo "  2. $0 list-artifacts dev    # See what's available"
    echo "  3. $0 deploy dev            # Deploy latest from S3 → EC2"
    echo "  4. $0 start dev             # Start application"
    echo "  5. $0 ping dev              # Verify it's responding"
    echo ""
    echo "Other examples:"
    echo "  $0 deploy dev abc123        # Deploy specific build-id"
    echo "  $0 logs dev abc123          # View build logs"
    echo "  $0 list-builds dev          # See recent builds"
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
        echo "Build ID: $build_id"
        echo "Artifact location: s3://${artifact_bucket}/releases/${build_id#*:}/"
        echo ""
        echo "Next step: $0 deploy $env"
    else
        echo "✗ CodeBuild failed with status: $status"
        echo ""
        echo "View logs with:"
        echo "  aws codebuild batch-get-builds --ids $build_id"
        exit 1
    fi
}

cmd_deploy() {
    local env=$1
    shift
    local build_id_param=""
    local key_file=""

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-file)
                key_file="$2"
                shift 2
                ;;
            *)
                # Assume first non-option arg is build-id
                if [ -z "$build_id_param" ]; then
                    build_id_param="$1"
                    shift
                else
                    echo "Unknown option: $1"
                    usage
                fi
                ;;
        esac
    done

    # Get stack outputs
    local artifact_bucket=$(get_stack_output "$env" "ArtifactBucketName")
    local instance_ip=$(get_instance_ip "$env") || exit 1
    key_file=$(get_key_file "$env" "$key_file") || exit 1

    if [ -z "$artifact_bucket" ]; then
        echo "Error: Could not get artifact bucket name from stack outputs"
        exit 1
    fi

    echo "=== Deploying Release from S3 to EC2 ==="
    echo "  Artifact Bucket: $artifact_bucket"
    echo "  EC2 Instance: $instance_ip"
    echo ""

    # Find artifact in S3
    local artifact_key
    if [ -n "$build_id_param" ]; then
        echo "Looking for build: $build_id_param"
        artifact_key=$(aws s3 ls "s3://${artifact_bucket}/releases/${build_id_param}/" --recursive | \
            grep "\.tar\.gz$" | awk '{print $4}' | head -1)
    else
        echo "Finding latest build..."
        # Find the most recent artifact
        artifact_key=$(aws s3 ls "s3://${artifact_bucket}/releases/" --recursive | \
            grep "\.tar\.gz$" | sort -r | head -1 | awk '{print $4}')
    fi

    if [ -z "$artifact_key" ]; then
        echo "✗ No release artifacts found in S3"
        echo ""
        if [ -n "$build_id_param" ]; then
            echo "Build ID '$build_id_param' not found."
        else
            echo "No builds exist yet. Run: $0 build $env"
        fi
        exit 1
    fi

    local tarball_name=$(basename "$artifact_key")
    echo "✓ Found artifact: $artifact_key"
    echo ""

    # Check if app is currently running
    if check_app_extracted "$instance_ip" "$key_file"; then
        echo "Checking if application is running..."
        if ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            -o ConnectTimeout=5 \
            "ec2-user@${instance_ip}" \
            "cd ${DEPLOY_DIR} && ./bin/${RELEASE_NAME} pid > /dev/null 2>&1"; then

            echo "⚠ Application is currently running - stopping it first..."
            ssh -i "$key_file" \
                -o StrictHostKeyChecking=no \
                -o UserKnownHostsFile=/dev/null \
                "ec2-user@${instance_ip}" \
                "cd ${DEPLOY_DIR} && ./bin/${RELEASE_NAME} stop" || true
            sleep 2
            echo "✓ Application stopped"
            echo ""
        fi
    fi

    # Download artifact to EC2
    echo "Deploying release to EC2..."
    ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        "ec2-user@${instance_ip}" bash <<EOF
set -e

cd ${DEPLOY_DIR}

echo "Downloading release from S3..."
aws s3 cp "s3://${artifact_bucket}/${artifact_key}" "${tarball_name}"

echo "Backing up current release..."
if [ -d "./bin" ]; then
    rm -rf ./backup
    mkdir -p ./backup
    mv bin lib releases erts-* ./backup/ 2>/dev/null || true
fi

echo "Extracting release..."
tar -xzf "${tarball_name}"

echo "✓ Release extracted successfully"
ls -lh ${tarball_name}
EOF

    echo ""
    echo "✓ Deployment complete!"
    echo ""
    echo "Next step: $0 start $env"
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

cmd_list_builds() {
    local env=$1

    local codebuild_project=$(get_stack_output "$env" "CodeBuildProjectName")

    if [ -z "$codebuild_project" ]; then
        echo "Error: Could not get CodeBuild project name from stack outputs"
        exit 1
    fi

    echo "Recent CodeBuild builds for: $codebuild_project"
    echo ""

    # Get list of builds for this project
    local build_ids=$(aws codebuild list-builds-for-project \
        --project-name "$codebuild_project" \
        --sort-order DESCENDING \
        --max-items 20 \
        --query 'ids' \
        --output text)

    if [ -z "$build_ids" ]; then
        echo "No builds found."
        return
    fi

    # Get details for these builds
    aws codebuild batch-get-builds \
        --ids $build_ids \
        --query 'builds[*].[id,buildStatus,startTime,endTime]' \
        --output table
}

cmd_list_artifacts() {
    local env=$1

    local artifact_bucket=$(get_stack_output "$env" "ArtifactBucketName")

    if [ -z "$artifact_bucket" ]; then
        echo "Error: Could not get artifact bucket name from stack outputs"
        exit 1
    fi

    echo "Available releases in S3: s3://${artifact_bucket}/releases/"
    echo ""

    # List all release artifacts
    local artifacts=$(aws s3 ls "s3://${artifact_bucket}/releases/" --recursive | grep "\.tar\.gz$")

    if [ -z "$artifacts" ]; then
        echo "No release artifacts found."
        echo ""
        echo "Run: $0 build $env"
        return
    fi

    echo "$artifacts" | while read -r date time size path; do
        local build_id=$(echo "$path" | sed 's|releases/\([^/]*\)/.*|\1|')
        local filename=$(basename "$path")
        printf "%-20s  %-10s  %s  %s\n" "$date $time" "$size" "$build_id" "$filename"
    done
}

cmd_logs() {
    local env=$1
    local build_id=$2

    if [ -z "$build_id" ]; then
        echo "Error: build-id required"
        echo "Usage: $0 logs <env> <build-id>"
        echo ""
        echo "Get build-id from: $0 list-builds $env"
        exit 1
    fi

    local codebuild_project=$(get_stack_output "$env" "CodeBuildProjectName")

    if [ -z "$codebuild_project" ]; then
        echo "Error: Could not get CodeBuild project name from stack outputs"
        exit 1
    fi

    echo "Fetching logs for build: $build_id"
    echo "---"
    echo ""

    local log_group="/aws/codebuild/${codebuild_project}"
    local log_stream="${build_id#*:}"

    # Fetch all log events
    aws logs get-log-events \
        --log-group-name "$log_group" \
        --log-stream-name "$log_stream" \
        --start-from-head \
        --output json 2>/dev/null | jq -r '.events[]?.message' 2>/dev/null

    if [ $? -ne 0 ]; then
        echo "Error: Could not fetch logs for build $build_id"
        echo ""
        echo "Verify build-id with: $0 list-builds $env"
        exit 1
    fi
}

cmd_init_status() {
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

    echo "Checking EC2 instance readiness on $instance_ip..."
    echo ""

    # Check if deployment directory exists
    if ssh -i "$key_file" \
        -o StrictHostKeyChecking=no \
        -o UserKnownHostsFile=/dev/null \
        -o ConnectTimeout=5 \
        "ec2-user@${instance_ip}" \
        "test -d ${DEPLOY_DIR}" 2>/dev/null; then

        echo "✓ Deployment directory exists: ${DEPLOY_DIR}"

        # Check if init log exists
        if ssh -i "$key_file" \
            -o StrictHostKeyChecking=no \
            -o UserKnownHostsFile=/dev/null \
            "ec2-user@${instance_ip}" \
            "test -f /var/log/hello-erlang-init.log" 2>/dev/null; then

            echo "✓ EC2 initialization complete"
            echo ""
            echo "Instance is ready. You can now run:"
            echo "  ./scripts/aws-deploy.sh build $env"
        else
            echo "⏳ EC2 initialization still in progress"
            echo ""
            echo "Wait a moment and try again."
        fi
    else
        echo "⏳ EC2 instance still initializing..."
        echo ""
        echo "UserData script is running. This usually takes 30-60 seconds."
        echo "Wait a moment and try again."
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
    deploy)
        cmd_deploy "$ENV" "$@"
        ;;
    list-builds)
        cmd_list_builds "$ENV" "$@"
        ;;
    list-artifacts)
        cmd_list_artifacts "$ENV" "$@"
        ;;
    logs)
        cmd_logs "$ENV" "$@"
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
    init-status)
        cmd_init_status "$ENV" "$@"
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac
