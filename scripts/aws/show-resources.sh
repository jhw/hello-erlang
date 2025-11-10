#!/bin/bash
# Show CloudFormation stack resources

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "Show CloudFormation stack resources."
    echo ""
    echo "Environments: dev, staging, prod"
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
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

STACK_NAME=$(get_stack_name "$ENV")

echo "Resources for: $STACK_NAME"
echo ""

aws cloudformation describe-stack-resources \
    --stack-name "$STACK_NAME" \
    --query 'StackResources[*].[LogicalResourceId,ResourceType,ResourceStatus,PhysicalResourceId]' \
    --output table
