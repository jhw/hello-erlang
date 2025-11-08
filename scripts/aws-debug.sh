#!/bin/bash
# Debugging and inspection tools for AWS resources

set -e

cd "$(dirname "$0")/.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 {list-builds|list-artifacts|logs|list-deployments|deployment-logs} <environment> [options]"
    echo ""
    echo "CodeBuild Commands:"
    echo "  list-builds <env>               - List recent CodeBuild builds"
    echo "  logs <env> <build-id>           - Show logs for a CodeBuild build"
    echo ""
    echo "S3 Commands:"
    echo "  list-artifacts <env>            - List available releases in S3"
    echo ""
    echo "CodeDeploy Commands:"
    echo "  list-deployments <env>          - List recent CodeDeploy deployments"
    echo "  deployment-logs <env> <dep-id>  - Show detailed deployment events"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Examples:"
    echo "  $0 list-builds dev              # See recent builds"
    echo "  $0 logs dev abc123              # View build logs"
    echo "  $0 list-artifacts dev           # Find previous build-id for rollback"
    echo "  $0 list-deployments dev         # See recent deployments"
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

# Command implementations
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
        echo "Run: ./scripts/aws-deploy.sh build $env"
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

cmd_list_deployments() {
    local env=$1

    local codedeploy_app=$(get_stack_output "$env" "CodeDeployApplicationName")
    local deployment_group=$(get_stack_output "$env" "DeploymentGroupName")

    if [ -z "$codedeploy_app" ] || [ -z "$deployment_group" ]; then
        echo "Error: Could not get CodeDeploy details from stack outputs"
        exit 1
    fi

    echo "Recent CodeDeploy deployments for: $codedeploy_app / $deployment_group"
    echo ""

    # List recent deployments
    local deployment_ids=$(aws deploy list-deployments \
        --application-name "$codedeploy_app" \
        --deployment-group-name "$deployment_group" \
        --max-items 20 \
        --query 'deployments' \
        --output text)

    if [ -z "$deployment_ids" ]; then
        echo "No deployments found."
        return
    fi

    # Get deployment details
    aws deploy batch-get-deployments \
        --deployment-ids $deployment_ids \
        --query 'deploymentsInfo[*].[deploymentId,status,createTime,completeTime]' \
        --output table
}

cmd_deployment_logs() {
    local env=$1
    local deployment_id=$2

    if [ -z "$deployment_id" ]; then
        echo "Error: deployment-id required"
        echo "Usage: $0 deployment-logs <env> <deployment-id>"
        echo ""
        echo "Get deployment-id from: $0 list-deployments $env"
        exit 1
    fi

    echo "Fetching deployment details for: $deployment_id"
    echo "---"
    echo ""

    # Get full deployment details
    aws deploy get-deployment \
        --deployment-id "$deployment_id" \
        --output json | jq

    echo ""
    echo "Lifecycle events:"
    echo "---"
    echo ""

    # Get instance deployment details
    aws deploy list-deployment-instances \
        --deployment-id "$deployment_id" \
        --query 'instancesList' \
        --output text | while read instance_id; do
        echo "Instance: $instance_id"
        aws deploy get-deployment-instance \
            --deployment-id "$deployment_id" \
            --instance-id "$instance_id" \
            --query 'instanceSummary.lifecycleEvents' \
            --output json | jq
    done
}

# Main command router
if [ -z "$1" ] || [ -z "$2" ]; then
    usage
fi

COMMAND=$1
ENV=$2
shift 2

case "$COMMAND" in
    list-builds)
        cmd_list_builds "$ENV"
        ;;
    list-artifacts)
        cmd_list_artifacts "$ENV"
        ;;
    logs)
        cmd_logs "$ENV" "$@"
        ;;
    list-deployments)
        cmd_list_deployments "$ENV"
        ;;
    deployment-logs)
        cmd_deployment_logs "$ENV" "$@"
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        echo ""
        usage
        ;;
esac
