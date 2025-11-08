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
    echo "Usage: $0 {build|rollback|deployment-status|ping|list-builds|list-artifacts|logs} <environment> [options]"
    echo ""
    echo "Build Commands:"
    echo "  build <env>                     - Build release in CodeBuild (auto-deploys via CodeDeploy)"
    echo "  list-builds <env>               - List recent CodeBuild builds"
    echo "  list-artifacts <env>            - List available releases in S3"
    echo "  logs <env> <build-id>           - Show logs for a CodeBuild build"
    echo ""
    echo "Deployment Commands:"
    echo "  rollback <env> <build-id>       - Deploy a specific previous build"
    echo "  deployment-status <env> [id]    - Check CodeDeploy deployment status"
    echo "  ping <env> [msg]                - Test application endpoint via ALB"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Deployment workflow:"
    echo "  1. $0 build dev                 # Build in CodeBuild → S3 → Auto-deploy via CodeDeploy"
    echo "  2. $0 deployment-status dev     # Check deployment status"
    echo "  3. $0 ping dev                  # Verify application is responding"
    echo ""
    echo "Rollback workflow:"
    echo "  1. $0 list-artifacts dev        # Find previous build-id"
    echo "  2. $0 rollback dev abc123       # Deploy previous build"
    echo ""
    echo "Other examples:"
    echo "  $0 logs dev abc123              # View build logs"
    echo "  $0 list-builds dev              # See recent builds"
    echo ""
    echo "NOTE: Deployments now happen automatically via AWS CodeDeploy."
    echo "      The 'deploy', 'start', 'stop', 'status' commands have been removed."
    echo "      Use 'rollback' to deploy a specific build or 'deployment-status' to check progress."
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
        echo "AWS CodeDeploy will automatically deploy this release to EC2."
        echo ""
        echo "Next steps:"
        echo "  $0 deployment-status $env     # Check deployment progress"
        echo "  $0 ping $env                  # Verify application is responding"
    else
        echo "✗ CodeBuild failed with status: $status"
        echo ""
        echo "View logs with:"
        echo "  aws codebuild batch-get-builds --ids $build_id"
        exit 1
    fi
}

cmd_rollback() {
    local env=$1
    local build_id=$2

    if [ -z "$build_id" ]; then
        echo "Error: build-id required"
        echo "Usage: $0 rollback <env> <build-id>"
        echo ""
        echo "Get build-id from: $0 list-artifacts $env"
        exit 1
    fi

    # Get stack outputs
    local artifact_bucket=$(get_stack_output "$env" "ArtifactBucketName")
    local codedeploy_app=$(get_stack_output "$env" "CodeDeployApplicationName")
    local deployment_group=$(get_stack_output "$env" "DeploymentGroupName")

    if [ -z "$artifact_bucket" ] || [ -z "$codedeploy_app" ] || [ -z "$deployment_group" ]; then
        echo "Error: Could not get required stack outputs"
        exit 1
    fi

    echo "=== Rolling back to previous build ==="
    echo "  Environment: $env"
    echo "  Build ID: $build_id"
    echo ""

    # Find artifact for build-id
    echo "Looking for artifact with build ID: $build_id"
    local artifact_key=$(aws s3 ls "s3://${artifact_bucket}/releases/${build_id}/" --recursive | \
        grep "\.tar\.gz$" | awk '{print $4}' | head -1)

    if [ -z "$artifact_key" ]; then
        echo "✗ Error: No artifact found for build ID: $build_id"
        echo ""
        echo "Available builds:"
        aws s3 ls "s3://${artifact_bucket}/releases/" --recursive | grep "\.tar\.gz$" | \
            awk '{print $4}' | sed 's|releases/\([^/]*\)/.*|\1|' | sort -u
        exit 1
    fi

    echo "✓ Found artifact: s3://${artifact_bucket}/${artifact_key}"
    echo ""

    # Create CodeDeploy deployment
    echo "Creating CodeDeploy deployment..."
    local deployment_id=$(aws deploy create-deployment \
        --application-name "$codedeploy_app" \
        --deployment-group-name "$deployment_group" \
        --s3-location bucket="$artifact_bucket",key="$artifact_key",bundleType=tgz \
        --description "Manual rollback to build: $build_id" \
        --query 'deploymentId' \
        --output text)

    if [ -z "$deployment_id" ]; then
        echo "✗ Error: Failed to create deployment"
        exit 1
    fi

    echo "✓ Deployment created: $deployment_id"
    echo ""
    echo "Monitoring deployment progress..."
    echo ""

    # Monitor deployment status
    local status="Created"
    while [[ "$status" != "Succeeded" && "$status" != "Failed" && "$status" != "Stopped" ]]; do
        sleep 5
        status=$(aws deploy get-deployment \
            --deployment-id "$deployment_id" \
            --query 'deploymentInfo.status' \
            --output text 2>/dev/null || echo "Unknown")

        echo "  Status: $status"
    done

    echo ""
    if [ "$status" == "Succeeded" ]; then
        echo "✓ Rollback completed successfully!"
        echo ""
        echo "Verify with: $0 ping $env"
    else
        echo "✗ Rollback failed with status: $status"
        echo ""
        echo "View deployment details:"
        echo "  aws deploy get-deployment --deployment-id $deployment_id"
        exit 1
    fi
}

cmd_deployment_status() {
    local env=$1
    local deployment_id=$2

    local codedeploy_app=$(get_stack_output "$env" "CodeDeployApplicationName")
    local deployment_group=$(get_stack_output "$env" "DeploymentGroupName")

    if [ -z "$codedeploy_app" ] || [ -z "$deployment_group" ]; then
        echo "Error: Could not get CodeDeploy information from stack outputs"
        exit 1
    fi

    # If no deployment ID provided, get the most recent one
    if [ -z "$deployment_id" ]; then
        echo "Finding most recent deployment..."
        deployment_id=$(aws deploy list-deployments \
            --application-name "$codedeploy_app" \
            --deployment-group-name "$deployment_group" \
            --max-items 1 \
            --query 'deployments[0]' \
            --output text 2>/dev/null)

        if [ -z "$deployment_id" ] || [ "$deployment_id" == "None" ]; then
            echo "No deployments found for environment '$env'"
            echo ""
            echo "Trigger a deployment with: $0 build $env"
            exit 0
        fi
    fi

    echo "=== CodeDeploy Deployment Status ==="
    echo "  Environment: $env"
    echo "  Deployment ID: $deployment_id"
    echo ""

    # Get full deployment info
    local deployment_info=$(aws deploy get-deployment \
        --deployment-id "$deployment_id" \
        --output json 2>/dev/null)

    if [ -z "$deployment_info" ]; then
        echo "Error: Could not retrieve deployment information"
        exit 1
    fi

    # Parse deployment details
    local status=$(echo "$deployment_info" | jq -r '.deploymentInfo.status')
    local description=$(echo "$deployment_info" | jq -r '.deploymentInfo.description // "N/A"')
    local create_time=$(echo "$deployment_info" | jq -r '.deploymentInfo.createTime')
    local complete_time=$(echo "$deployment_info" | jq -r '.deploymentInfo.completeTime // "In progress"')

    echo "Status: $status"
    echo "Description: $description"
    echo "Created: $(date -d @$create_time 2>/dev/null || date -r $create_time)"
    if [ "$complete_time" != "In progress" ]; then
        echo "Completed: $(date -d @$complete_time 2>/dev/null || date -r $complete_time)"
    fi
    echo ""

    # Show lifecycle events
    echo "Lifecycle Events:"
    aws deploy get-deployment \
        --deployment-id "$deployment_id" \
        --query 'deploymentInfo.instancesSummary' \
        --output table

    if [ "$status" == "Succeeded" ]; then
        echo ""
        echo "✓ Deployment successful!"
        echo ""
        echo "Verify application: $0 ping $env"
    elif [ "$status" == "Failed" ] || [ "$status" == "Stopped" ]; then
        echo ""
        echo "✗ Deployment $status"
        echo ""
        echo "View detailed error information:"
        echo "  aws deploy get-deployment --deployment-id $deployment_id"
    elif [ "$status" == "InProgress" ] || [ "$status" == "Created" ]; then
        echo ""
        echo "⏳ Deployment in progress..."
        echo ""
        echo "Check again in a few moments with:"
        echo "  $0 deployment-status $env $deployment_id"
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
    rollback)
        cmd_rollback "$ENV" "$@"
        ;;
    deployment-status)
        cmd_deployment_status "$ENV" "$@"
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
    ping)
        cmd_ping "$ENV" "$@"
        ;;
    # Deprecated commands - removed in favor of CodeDeploy automation
    deploy|start|stop|restart|status|init-status)
        echo "Error: The '$COMMAND' command has been removed."
        echo ""
        echo "This project now uses AWS CodeDeploy for automated deployments."
        echo "Deployments happen automatically when you run: $0 build $ENV"
        echo ""
        echo "Available commands:"
        echo "  build                  - Build and automatically deploy"
        echo "  rollback <build-id>    - Deploy a specific previous build"
        echo "  deployment-status      - Check deployment progress"
        echo "  ping                   - Test application endpoint"
        echo ""
        echo "For help: $0 --help"
        exit 1
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac
