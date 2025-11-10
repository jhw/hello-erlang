#!/bin/bash
# Show CodeBuild logs for a specific build

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment> <build-id>"
    echo ""
    echo "Show CodeBuild logs for a specific build."
    echo ""
    echo "Environments: dev, staging, prod"
    echo "Build ID: Get from list-builds.sh output"
    echo ""
    echo "Example:"
    echo "  $0 dev dev-hello-erlang-build:abc123def"
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

get_stack_output() {
    local env=$1
    local output_key=$2
    local stack_name=$(get_stack_name "$env")

    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query "Stacks[0].Outputs[?OutputKey=='$output_key'].OutputValue" \
        --output text 2>/dev/null
}

if [ $# -lt 2 ]; then
    usage
fi

ENV=$1
BUILD_ID=$2

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        exit 1
        ;;
esac

CODEBUILD_PROJECT=$(get_stack_output "$ENV" "CodeBuildProjectName")

if [ -z "$CODEBUILD_PROJECT" ]; then
    echo "Error: Could not get CodeBuild project name from stack outputs"
    exit 1
fi

echo "Fetching logs for build: $BUILD_ID"
echo "---"
echo ""

LOG_GROUP="/aws/codebuild/${CODEBUILD_PROJECT}"
LOG_STREAM="${BUILD_ID#*:}"

# Fetch all log events
aws logs get-log-events \
    --log-group-name "$LOG_GROUP" \
    --log-stream-name "$LOG_STREAM" \
    --start-from-head \
    --output json 2>/dev/null | jq -r '.events[]?.message' 2>/dev/null

if [ $? -ne 0 ]; then
    echo "Error: Could not fetch logs for build $BUILD_ID"
    echo ""
    echo "Verify build-id with: ./scripts/aws/list-builds.sh $ENV"
    exit 1
fi
