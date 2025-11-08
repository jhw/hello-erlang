#!/bin/bash
# CodeDeploy lifecycle hook: ValidateService
# Validates that the application is running and responding to requests

set -e

DEPLOY_DIR="/opt/hello_erlang"
RELEASE_NAME="hello_erlang"
APP_PORT="8080"
HEALTH_ENDPOINT="http://localhost:${APP_PORT}/echo?message=health"

echo "=== ValidateService Hook ==="
echo "Validating application health..."

cd "$DEPLOY_DIR"

# Check if application process is running
if ! ./bin/${RELEASE_NAME} pid > /dev/null 2>&1; then
    echo "✗ Error: Application process is not running"
    exit 1
fi

PID=$(./bin/${RELEASE_NAME} pid)
echo "✓ Application process running (PID: $PID)"

# Test the HTTP endpoint
echo "Testing HTTP endpoint: $HEALTH_ENDPOINT"

# Wait up to 30 seconds for application to be ready
MAX_ATTEMPTS=15
ATTEMPT=0

while [ $ATTEMPT -lt $MAX_ATTEMPTS ]; do
    if command -v curl > /dev/null 2>&1; then
        RESPONSE=$(curl -s -w "\n%{http_code}" --connect-timeout 2 --max-time 5 "$HEALTH_ENDPOINT" 2>/dev/null || echo -e "\n000")
        HTTP_CODE=$(echo "$RESPONSE" | tail -n 1)
        BODY=$(echo "$RESPONSE" | sed '$d')

        if [ "$HTTP_CODE" == "200" ] && [ "$BODY" == "health" ]; then
            echo "✓ Application is responding correctly"
            echo "  Status: 200 OK"
            echo "  Response: $BODY"
            exit 0
        fi
    fi

    ATTEMPT=$((ATTEMPT + 1))
    if [ $ATTEMPT -lt $MAX_ATTEMPTS ]; then
        echo "  Attempt $ATTEMPT/$MAX_ATTEMPTS failed, retrying in 2 seconds..."
        sleep 2
    fi
done

# If we got here, validation failed
echo "✗ Error: Application is not responding correctly after $MAX_ATTEMPTS attempts"
if [ -n "$HTTP_CODE" ] && [ "$HTTP_CODE" != "000" ]; then
    echo "  Last HTTP status: $HTTP_CODE"
    echo "  Last response: $BODY"
fi

# Show recent logs for debugging
if [ -d "./log" ]; then
    echo ""
    echo "Recent log entries:"
    tail -n 30 ./log/*.log 2>/dev/null || true
fi

exit 1
