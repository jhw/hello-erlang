#!/bin/bash
# Restart CloudWatch and CodeDeploy agents on EC2 instance

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "Restart CloudWatch and CodeDeploy agents on the EC2 instance."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "This uses AWS Systems Manager (SSM) to execute commands remotely."
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

echo "Restarting CloudWatch and CodeDeploy agents..."
echo "  Environment: $ENV"
echo ""

# Get EC2 instance ID from stack
INSTANCE_ID=$(aws cloudformation describe-stack-resources \
    --stack-name "$STACK_NAME" \
    --query 'StackResources[?ResourceType==`AWS::EC2::Instance`].PhysicalResourceId' \
    --output text 2>/dev/null)

if [ -z "$INSTANCE_ID" ]; then
    echo "Error: Could not find EC2 instance for environment '$ENV'" >&2
    exit 1
fi

echo "Instance ID: $INSTANCE_ID"
echo ""

# Restart CloudWatch agent
echo "Restarting CloudWatch agent..."
aws ssm send-command \
    --instance-ids "$INSTANCE_ID" \
    --document-name "AWS-RunShellScript" \
    --parameters 'commands=["sudo systemctl restart amazon-cloudwatch-agent"]' \
    --output text \
    --query 'Command.CommandId' > /dev/null

echo "  ✓ CloudWatch agent restart initiated"

# Restart CodeDeploy agent
echo "Restarting CodeDeploy agent..."
aws ssm send-command \
    --instance-ids "$INSTANCE_ID" \
    --document-name "AWS-RunShellScript" \
    --parameters 'commands=["sudo systemctl restart codedeploy-agent"]' \
    --output text \
    --query 'Command.CommandId' > /dev/null

echo "  ✓ CodeDeploy agent restart initiated"

echo ""
echo "✓ Agent restarts initiated"
echo ""
echo "Note: It may take a few seconds for the agents to fully restart."
echo "Check status: ./scripts/aws/show-instance-logs.sh $ENV"
