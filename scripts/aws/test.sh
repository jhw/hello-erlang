#!/bin/bash
# Test deployed hello-erlang application endpoints
#
# Usage:
#   ./scripts/aws/test.sh echo <env> [message]
#   ./scripts/aws/test.sh add <env> <x> <y>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Source environment configuration
if [ -f "$PROJECT_ROOT/config/env.sh" ]; then
    source "$PROJECT_ROOT/config/env.sh"
fi

get_stack_name() {
    echo "hello-erlang-$1"
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
        echo "Make sure the stack is deployed: ./scripts/aws/stack.sh deploy $env" >&2
        exit 1
    fi

    echo "$alb_dns"
}

test_echo() {
    local env=$1
    local message="${2:-ping}"

    local alb_dns=$(get_alb_dns $env)

    # URL encode the message
    local encoded_message=$(printf %s "$message" | jq -sRr @uri)
    local url="http://${alb_dns}/echo?message=${encoded_message}"

    echo "Testing /echo endpoint..."
    echo "  Environment: $env"
    echo "  URL: $url"
    echo "  Message: $message"
    echo ""

    if ! command -v curl &> /dev/null; then
        echo "Error: curl not found. Please install curl."
        exit 1
    fi

    local response=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$url" 2>/dev/null)
    local http_code=$(echo "$response" | tail -n 1)
    local body=$(echo "$response" | sed '$d')

    if [ "$http_code" == "200" ] && [ "$body" == "$message" ]; then
        echo "✓ Echo endpoint is working"
        echo "  Status: 200 OK"
        echo "  Response: $body"
    elif [ "$http_code" == "200" ]; then
        echo "⚠ Echo endpoint responded but with unexpected content"
        echo "  Status: 200 OK"
        echo "  Response: $body"
        echo "  Expected: $message"
    elif [ -n "$http_code" ]; then
        echo "✗ Echo endpoint returned error"
        echo "  Status: $http_code"
        echo "  Response: $body"
        exit 1
    else
        echo "✗ Echo endpoint not responding"
        echo "  Could not connect to $url"
        exit 1
    fi
}

test_add() {
    local env=$1
    local x=$2
    local y=$3

    if [ -z "$x" ] || [ -z "$y" ]; then
        echo "Error: Both x and y parameters required for add test"
        echo "Usage: $0 add <env> <x> <y>"
        exit 1
    fi

    local alb_dns=$(get_alb_dns $env)
    local url="http://${alb_dns}/add?x=${x}&y=${y}"

    echo "Testing /add endpoint..."
    echo "  Environment: $env"
    echo "  URL: $url"
    echo "  Parameters: x=$x, y=$y"
    echo ""

    if ! command -v curl &> /dev/null; then
        echo "Error: curl not found. Please install curl."
        exit 1
    fi

    local response=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$url" 2>/dev/null)
    local http_code=$(echo "$response" | tail -n 1)
    local body=$(echo "$response" | sed '$d')

    # Check if inputs are valid integers
    if [[ "$x" =~ ^-?[0-9]+$ ]] && [[ "$y" =~ ^-?[0-9]+$ ]]; then
        # Valid integers - expect success
        local expected=$((x + y))

        if [ "$http_code" == "200" ] && [ "$body" == "$expected" ]; then
            echo "✓ Add endpoint is working"
            echo "  Status: 200 OK"
            echo "  Result: $x + $y = $body"
        elif [ "$http_code" == "200" ]; then
            echo "⚠ Add endpoint responded but with unexpected result"
            echo "  Status: 200 OK"
            echo "  Response: $body"
            echo "  Expected: $expected"
        else
            echo "✗ Add endpoint returned error"
            echo "  Status: ${http_code:-connection failed}"
            echo "  Response: $body"
            exit 1
        fi
    else
        # Invalid inputs - expect crash/error (500)
        if [ "$http_code" == "500" ] || [ "$http_code" == "503" ]; then
            echo "✓ Add endpoint crashed as expected (invalid input)"
            echo "  Status: $http_code"
            echo "  This should trigger an error alert in Slack!"
            echo ""
            echo "Check Slack channel for error notification from:"
            echo "  AppErrorNotifierFunction → $env environment"
        elif [ "$http_code" == "200" ]; then
            echo "⚠ Add endpoint returned 200, but expected crash on invalid input"
            echo "  Response: $body"
        elif [ -n "$http_code" ]; then
            echo "✗ Add endpoint returned unexpected error"
            echo "  Status: $http_code"
            echo "  Response: $body"
        else
            echo "✗ Add endpoint not responding"
            echo "  Could not connect to $url"
            exit 1
        fi
    fi
}

usage() {
    cat << EOF
Test deployed hello-erlang application endpoints

Usage:
  $0 echo <env> [message]     Test echo endpoint (default message: "ping")
  $0 add <env> <x> <y>        Test add endpoint with two numbers

Examples:
  # Test echo with default "ping" message
  $0 echo dev

  # Test echo with custom message
  $0 echo dev "hello world"

  # Test add with valid integers (should succeed)
  $0 add dev 5 10

  # Test add with invalid input (should crash and trigger Slack alert)
  $0 add dev foo bar

Environments: dev, staging, prod
EOF
    exit 1
}

# Main command router
case "$1" in
    echo)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        test_echo "$2" "$3"
        ;;
    add)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        test_add "$2" "$3" "$4"
        ;;
    *)
        usage
        ;;
esac
