#!/bin/bash
# Show EC2 instance UserData logs via SSM

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "Show EC2 instance UserData execution logs via AWS Systems Manager."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "This shows the initialization logs from /var/log/hello-erlang-init.log"
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

# Get EC2 instance ID from stack
INSTANCE_ID=$(aws cloudformation describe-stack-resources \
    --stack-name "$STACK_NAME" \
    --query 'StackResources[?ResourceType==`AWS::EC2::Instance`].PhysicalResourceId' \
    --output text 2>/dev/null)

if [ -z "$INSTANCE_ID" ]; then
    echo "Error: Could not find EC2 instance for environment '$ENV'" >&2
    exit 1
fi

echo "Fetching UserData logs from instance: $INSTANCE_ID"
echo "---"
echo ""

# Send command to read the log file
COMMAND_ID=$(aws ssm send-command \
    --instance-ids "$INSTANCE_ID" \
    --document-name "AWS-RunShellScript" \
    --parameters 'commands=["cat /var/log/hello-erlang-init.log 2>/dev/null || echo \"Log file not found\""]' \
    --output text \
    --query 'Command.CommandId')

# Wait for command to complete
sleep 2

# Get command output
aws ssm get-command-invocation \
    --command-id "$COMMAND_ID" \
    --instance-id "$INSTANCE_ID" \
    --output text \
    --query 'StandardOutputContent'
