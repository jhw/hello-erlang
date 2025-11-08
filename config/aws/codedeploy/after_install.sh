#!/bin/bash
# CodeDeploy lifecycle hook: AfterInstall
# Validates the release structure and sets correct permissions

set -e

DEPLOY_DIR="/opt/hello_erlang"
RELEASE_NAME="hello_erlang"

echo "=== AfterInstall Hook ==="
echo "Validating release structure..."

cd "$DEPLOY_DIR"

# Check for required directories
for dir in bin lib releases; do
    if [ ! -d "$dir" ]; then
        echo "✗ Error: Required directory missing: $dir"
        exit 1
    fi
done

# Check for release executable
if [ ! -f "./bin/${RELEASE_NAME}" ]; then
    echo "✗ Error: Release executable not found: ./bin/${RELEASE_NAME}"
    exit 1
fi

# Set correct permissions (ignore errors if already set)
echo "Setting permissions..."
chmod +x ./bin/* 2>/dev/null || true
chmod -R u+rw,g+r,o+r . 2>/dev/null || true

echo "✓ Release structure validated"
echo "✓ Permissions set correctly"

# Display release info
echo ""
echo "Release contents:"
ls -lh ./bin/${RELEASE_NAME}

exit 0
