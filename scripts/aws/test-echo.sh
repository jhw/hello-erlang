#!/bin/bash
# Test echo endpoint

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment> [message]"
    echo ""
    echo "Test the /echo endpoint of the deployed application."
    echo ""
    echo "Environments: dev, staging, prod"
    echo "Message: Text to echo (default: 'ping')"
    echo ""
    echo "Example:"
    echo "  $0 dev hello"
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

get_alb_dns() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    local alb_dns=$(aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query 'Stacks[0].Outputs[?OutputKey==`LoadBalancerDNS`].OutputValue' \
        --output text 2>/dev/null)

    if [ -z "$alb_dns" ]; then
        echo "Error: Could not get Load Balancer DNS for environment '$env'" >&2
        echo "Make sure the stack is deployed: ./scripts/aws/deploy-stack.sh $env" >&2
        exit 1
    fi

    echo "$alb_dns"
}

if [ $# -lt 1 ]; then
    usage
fi

ENV=$1
MESSAGE="${2:-ping}"

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        exit 1
        ;;
esac

ALB_DNS=$(get_alb_dns "$ENV")

# URL encode the message
ENCODED_MESSAGE=$(printf %s "$MESSAGE" | jq -sRr @uri)
URL="http://${ALB_DNS}/echo?message=${ENCODED_MESSAGE}"

echo "Testing /echo endpoint..."
echo "  Environment: $ENV"
echo "  URL: $URL"
echo "  Message: $MESSAGE"
echo ""

if ! command -v curl &> /dev/null; then
    echo "Error: curl not found. Please install curl."
    exit 1
fi

RESPONSE=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$URL" 2>/dev/null)
HTTP_CODE=$(echo "$RESPONSE" | tail -n 1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" == "200" ] && [ "$BODY" == "$MESSAGE" ]; then
    echo "✓ Echo endpoint is working"
    echo "  Status: 200 OK"
    echo "  Response: $BODY"
elif [ "$HTTP_CODE" == "200" ]; then
    echo "⚠ Echo endpoint responded but with unexpected content"
    echo "  Status: 200 OK"
    echo "  Response: $BODY"
    echo "  Expected: $MESSAGE"
elif [ -n "$HTTP_CODE" ]; then
    echo "✗ Echo endpoint returned error"
    echo "  Status: $HTTP_CODE"
    echo "  Response: $BODY"
    exit 1
else
    echo "✗ Echo endpoint not responding"
    echo "  Could not connect to $URL"
    exit 1
fi
