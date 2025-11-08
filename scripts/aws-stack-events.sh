#!/bin/bash
# Show detailed CloudFormation stack events with failure reasons

set -e

cd "$(dirname "$0")/.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment> [max-items]"
    echo ""
    echo "Shows CloudFormation stack events with full failure reasons"
    echo ""
    echo "Arguments:"
    echo "  environment    - dev, staging, or prod"
    echo "  max-items      - Number of events to show (default: 50)"
    echo ""
    echo "Examples:"
    echo "  $0 dev         # Show last 50 events"
    echo "  $0 dev 100     # Show last 100 events"
    exit 1
}

if [ -z "$1" ]; then
    usage
fi

ENV=$1
MAX_ITEMS=${2:-50}
STACK_NAME="${STACK_PREFIX}-${ENV}"

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
    ] |
    @tsv
' | while IFS=$'\t' read -r timestamp status type resource reason; do
    # Color code based on status
    case "$status" in
        *FAILED*|*ROLLBACK*)
            color="\033[31m"  # Red
            ;;
        *COMPLETE*)
            color="\033[32m"  # Green
            ;;
        *IN_PROGRESS*)
            color="\033[33m"  # Yellow
            ;;
        *)
            color="\033[0m"   # Default
            ;;
    esac

    reset="\033[0m"

    # Format output
    printf "${color}%-25s %-20s %-35s %s${reset}\n" "$timestamp" "$status" "$resource" "$type"

    # Show reason on next line if present and it's a failure
    if [ -n "$reason" ] && [[ "$status" == *"FAILED"* ]]; then
        printf "  ${color}└─ Reason: %s${reset}\n" "$reason"
    fi
done

echo ""
echo "Legend:"
echo -e "  \033[32m■\033[0m Success (COMPLETE)"
echo -e "  \033[33m■\033[0m In Progress"
echo -e "  \033[31m■\033[0m Failed or Rollback"
echo ""
echo "To see more events: $0 $ENV 100"
