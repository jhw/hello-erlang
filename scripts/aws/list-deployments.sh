#!/bin/bash
# List recent CodeDeploy deployments

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "List recent CodeDeploy deployments."
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

CODEDEPLOY_APP=$(get_stack_output "$ENV" "CodeDeployApplicationName")
DEPLOYMENT_GROUP=$(get_stack_output "$ENV" "DeploymentGroupName")

if [ -z "$CODEDEPLOY_APP" ] || [ -z "$DEPLOYMENT_GROUP" ]; then
    echo "Error: Could not get CodeDeploy details from stack outputs"
    exit 1
fi

echo "Recent CodeDeploy deployments for: $CODEDEPLOY_APP / $DEPLOYMENT_GROUP"
echo ""

# List recent deployments
DEPLOYMENT_IDS=$(aws deploy list-deployments \
    --application-name "$CODEDEPLOY_APP" \
    --deployment-group-name "$DEPLOYMENT_GROUP" \
    --max-items 20 \
    --query 'deployments' \
    --output text)

if [ -z "$DEPLOYMENT_IDS" ]; then
    echo "No deployments found."
    exit 0
fi

# Get deployment details
aws deploy batch-get-deployments \
    --deployment-ids $DEPLOYMENT_IDS \
    --query 'deploymentsInfo[*].[deploymentId,status,createTime,completeTime]' \
    --output table
