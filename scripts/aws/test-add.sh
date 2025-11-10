#!/bin/bash
# Test add endpoint

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment> <x> <y>"
    echo ""
    echo "Test the /add endpoint of the deployed application."
    echo ""
    echo "Environments: dev, staging, prod"
    echo "x, y: Numbers to add"
    echo ""
    echo "Example:"
    echo "  $0 dev 5 7      # Should return 12"
    echo "  $0 dev 10 -3    # Should return 7"
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

if [ $# -lt 3 ]; then
    usage
fi

ENV=$1
X=$2
Y=$3

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        exit 1
        ;;
esac

ALB_DNS=$(get_alb_dns "$ENV")
URL="http://${ALB_DNS}/add?x=${X}&y=${Y}"

echo "Testing /add endpoint..."
echo "  Environment: $ENV"
echo "  URL: $URL"
echo "  Parameters: x=$X, y=$Y"
echo ""

if ! command -v curl &> /dev/null; then
    echo "Error: curl not found. Please install curl."
    exit 1
fi

RESPONSE=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$URL" 2>/dev/null)
HTTP_CODE=$(echo "$RESPONSE" | tail -n 1)
BODY=$(echo "$RESPONSE" | sed '$d')

# Check if inputs are valid integers
if [[ "$X" =~ ^-?[0-9]+$ ]] && [[ "$Y" =~ ^-?[0-9]+$ ]]; then
    # Valid integers - expect success
    EXPECTED=$((X + Y))

    if [ "$HTTP_CODE" == "200" ] && [ "$BODY" == "$EXPECTED" ]; then
        echo "✓ Add endpoint is working"
        echo "  Status: 200 OK"
        echo "  Response: $BODY"
        echo "  Calculation: $X + $Y = $EXPECTED"
    elif [ "$HTTP_CODE" == "200" ]; then
        echo "✗ Add endpoint returned wrong result"
        echo "  Status: 200 OK"
        echo "  Response: $BODY"
        echo "  Expected: $EXPECTED"
        exit 1
    elif [ -n "$HTTP_CODE" ]; then
        echo "✗ Add endpoint returned error"
        echo "  Status: $HTTP_CODE"
        echo "  Response: $BODY"
        exit 1
    else
        echo "✗ Add endpoint not responding"
        echo "  Could not connect to $URL"
        exit 1
    fi
else
    # Invalid inputs - expect error
    if [ "$HTTP_CODE" == "400" ] || [ "$HTTP_CODE" == "500" ]; then
        echo "✓ Add endpoint correctly rejected invalid input"
        echo "  Status: $HTTP_CODE"
        echo "  Response: $BODY"
        echo "  (This is expected for invalid inputs)"
    elif [ "$HTTP_CODE" == "200" ]; then
        echo "⚠ Add endpoint accepted invalid input"
        echo "  Status: 200 OK"
        echo "  Response: $BODY"
        echo "  (Should have returned an error for non-integer inputs)"
    elif [ -n "$HTTP_CODE" ]; then
        echo "✗ Add endpoint returned unexpected error"
        echo "  Status: $HTTP_CODE"
        echo "  Response: $BODY"
        exit 1
    else
        echo "✗ Add endpoint not responding"
        echo "  Could not connect to $URL"
        exit 1
    fi
fi
