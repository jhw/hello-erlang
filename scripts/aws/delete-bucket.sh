#!/bin/bash
# Delete stack-artifacts S3 bucket

set -e

cd "$(dirname "$0")/../.."

if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "Delete stack-artifacts S3 bucket."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Warning: This will empty the bucket first, deleting all objects and versions."
    exit 1
}

get_bucket_name() {
    local env=$1
    echo "${env}-${STACK_PREFIX}-stack-artifacts-${ACCOUNT_ID}"
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

BUCKET_NAME=$(get_bucket_name "$ENV")

echo "Deleting stack artifacts bucket: $BUCKET_NAME"

# Check if bucket exists
if ! aws s3api head-bucket --bucket "$BUCKET_NAME" 2>/dev/null; then
    echo "Bucket does not exist: $BUCKET_NAME"
    exit 0
fi

# Empty bucket first (including all versions)
echo "Emptying bucket (this may take a moment)..."
./scripts/aws/empty-bucket.sh "$ENV"

# Delete bucket
echo "Deleting bucket..."
aws s3api delete-bucket --bucket "$BUCKET_NAME"

echo "✓ Bucket deleted successfully: $BUCKET_NAME"
