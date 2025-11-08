#!/bin/bash
# Application deployment to EC2

set -e

cd "$(dirname "$0")/.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
DEPLOY_DIR="${DEPLOY_DIR:-/opt/hello_erlang}"
RELEASE_NAME="hello_erlang"

usage() {
    echo "Usage: $0 {build|ping} <environment> [options]"
    echo ""
    echo "Build Commands:"
    echo "  build <env>         - Build release in CodeBuild (auto-deploys via CodeDeploy)"
    echo "  ping <env> [msg]    - Test application endpoint via ALB"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Deployment workflow:"
    echo "  1. $0 build dev     # Build in CodeBuild → S3 → Auto-deploy via CodeDeploy"
    echo "  2. $0 ping dev      # Verify application is responding"
    echo ""
    echo "Debugging (use aws-debug.sh):"
    echo "  ./scripts/aws-debug.sh list-builds dev         # List recent builds"
    echo "  ./scripts/aws-debug.sh logs dev <build-id>     # View build logs"
    echo "  ./scripts/aws-debug.sh list-artifacts dev      # List available artifacts"
    echo "  ./scripts/aws-debug.sh list-deployments dev    # List recent deployments"
    echo "  ./scripts/aws-debug.sh deployment-logs dev <id> # View deployment details"
    echo ""
    echo "NOTE: Deployments happen automatically via AWS CodeDeploy."
    echo "      Use AWS Console or CodeDeploy API for manual rollback operations."
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

# Removed SSH-based helper functions (get_instance_ip, get_key_file, check_app_extracted)
# These are no longer needed with CodeDeploy automation

# Command implementations
cmd_build() {
    local env=$1
    shift

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

    # Copy buildspec.yml to root for CodeBuild (expects it at root)
    cp config/aws/buildspec.yml buildspec.yml

    zip -q -r "$source_bundle" \
        apps buildspec.yml config rebar.config rebar.lock \
        -x "*.git*" "*_build*" "*.rebar3*" "*.beam" "config/aws/stack.yaml" "config/env.sh"

    # Clean up temporary buildspec copy
    rm buildspec.yml

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
        echo "AWS CodeDeploy will automatically deploy this release to EC2."
        echo ""
        echo "Next steps:"
        echo "  $0 ping $env                                # Verify application is responding"
        echo "  ./scripts/aws-debug.sh list-deployments $env  # Check deployment status"
    else
        echo "✗ CodeBuild failed with status: $status"
        echo ""
        echo "View logs with:"
        echo "  aws codebuild batch-get-builds --ids $build_id"
        exit 1
    fi
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
    ping)
        cmd_ping "$ENV" "$@"
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        echo ""
        usage
        ;;
esac
