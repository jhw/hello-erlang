#!/bin/bash
# List recent CodeBuild builds

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "List recent CodeBuild builds."
    echo ""
    echo "Environments: dev, staging, prod"
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

if [ $# -lt 1 ]; then
    usage
fi

ENV=$1

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

echo "Recent CodeBuild builds for: $CODEBUILD_PROJECT"
echo ""

# Get list of builds for this project
BUILD_IDS=$(aws codebuild list-builds-for-project \
    --project-name "$CODEBUILD_PROJECT" \
    --sort-order DESCENDING \
    --max-items 20 \
    --query 'ids' \
    --output text)

if [ -z "$BUILD_IDS" ]; then
    echo "No builds found."
    exit 0
fi

# Get details for these builds
aws codebuild batch-get-builds \
    --ids $BUILD_IDS \
    --query 'builds[*].[id,buildStatus,startTime,endTime]' \
    --output table
