#!/bin/bash
# CodeDeploy lifecycle hook: ApplicationStop
# Stops the running Erlang application gracefully

set -e

DEPLOY_DIR="/opt/hello_erlang"
RELEASE_NAME="hello_erlang"

echo "=== ApplicationStop Hook ==="
echo "Stopping Erlang application if running..."

cd "$DEPLOY_DIR"

if [ -f "./bin/${RELEASE_NAME}" ]; then
    # Check if application is running
    if ./bin/${RELEASE_NAME} pid > /dev/null 2>&1; then
        PID=$(./bin/${RELEASE_NAME} pid)
        echo "Application is running (PID: $PID), stopping..."
        ./bin/${RELEASE_NAME} stop || true
        sleep 2
        echo "✓ Application stopped"
    else
        echo "Application is not running, nothing to stop"
    fi
else
    echo "No existing release found, nothing to stop"
fi

exit 0
