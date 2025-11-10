#!/bin/bash
# CodeDeploy lifecycle hook: BeforeInstall
# Prepares the deployment directory by backing up the current release

set -e

DEPLOY_DIR="/opt/hello_erlang"
LOG_DIR="/var/log/hello_erlang"

echo "=== BeforeInstall Hook ==="
echo "Preparing deployment directory: $DEPLOY_DIR"

# Create log directory if it doesn't exist
if [ ! -d "$LOG_DIR" ]; then
    echo "Creating log directory: $LOG_DIR"
    sudo mkdir -p "$LOG_DIR"
    sudo chown ec2-user:ec2-user "$LOG_DIR"
    sudo chmod 755 "$LOG_DIR"
    echo "✓ Log directory created"
else
    echo "Log directory already exists: $LOG_DIR"
fi

cd "$DEPLOY_DIR"

# Backup current release if it exists
if [ -d "./bin" ]; then
    echo "Backing up current release..."
    rm -rf ./backup
    mkdir -p ./backup

    # Move release directories to backup
    for dir in bin lib releases erts-*; do
        if [ -e "$dir" ]; then
            mv "$dir" ./backup/ 2>/dev/null || true
        fi
    done

    echo "✓ Current release backed up to ./backup/"
else
    echo "No existing release found, skipping backup"
fi

# Clean up any old tarballs
echo "Cleaning up old tarballs..."
rm -f *.tar.gz

echo "✓ Directory prepared for new release"
exit 0
