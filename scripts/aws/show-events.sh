#!/bin/bash
# Show CloudFormation stack events

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment> [max-items]"
    echo ""
    echo "Show CloudFormation stack events."
    echo ""
    echo "Environments: dev, staging, prod"
    echo "Max items: Number of events to show (default: 50)"
    echo ""
    echo "Example:"
    echo "  $0 dev 20    # Show last 20 events"
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
MAX_ITEMS=${2:-50}

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        exit 1
        ;;
esac

STACK_NAME=$(get_stack_name "$ENV")

echo "CloudFormation events for: $STACK_NAME"
echo "Showing last $MAX_ITEMS events"
echo ""

# Get events with full details including ResourceStatusReason
aws cloudformation describe-stack-events \
    --stack-name "$STACK_NAME" \
    --max-items "$MAX_ITEMS" \
    --output json | jq -r '
    .StackEvents[] |
    [
        .Timestamp,
        .ResourceStatus,
        .ResourceType,
        .LogicalResourceId,
        (.ResourceStatusReason // "")
    ] | @tsv' | \
    column -t -s $'\t'
