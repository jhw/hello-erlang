#!/bin/bash
# CodeDeploy lifecycle hook: ApplicationStart
# Starts the Erlang application in daemon mode

set -e

DEPLOY_DIR="/opt/hello_erlang"
RELEASE_NAME="hello_erlang"

echo "=== ApplicationStart Hook ==="
echo "Starting Erlang application..."

cd "$DEPLOY_DIR"

if [ ! -f "./bin/${RELEASE_NAME}" ]; then
    echo "✗ Error: Release executable not found"
    exit 1
fi

# Start the release in daemon mode
./bin/${RELEASE_NAME} daemon

# Wait for application to start
echo "Waiting for application to start..."
sleep 3

# Verify it started
if ./bin/${RELEASE_NAME} pid > /dev/null 2>&1; then
    PID=$(./bin/${RELEASE_NAME} pid)
    echo "✓ Application started successfully (PID: $PID)"
    exit 0
else
    echo "✗ Error: Application failed to start"

    # Try to get logs for debugging
    if [ -d "./log" ]; then
        echo "Recent log entries:"
        tail -n 20 ./log/*.log 2>/dev/null || true
    fi

    exit 1
fi
