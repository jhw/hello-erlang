#!/bin/bash
# Empty stack-artifacts S3 bucket

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
    echo "Empty stack-artifacts S3 bucket (delete all objects and versions)."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Note: The bucket itself is not deleted, only its contents."
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

echo "Emptying bucket: $BUCKET_NAME"

# Check if bucket exists
if ! aws s3api head-bucket --bucket "$BUCKET_NAME" 2>/dev/null; then
    echo "Bucket does not exist: $BUCKET_NAME"
    exit 0
fi

# Delete all object versions
aws s3api list-object-versions \
    --bucket "$BUCKET_NAME" \
    --output json \
    --query '{Objects: Versions[].{Key:Key,VersionId:VersionId}}' | \
jq -r '.Objects[]? | "\(.Key)\t\(.VersionId)"' | \
while IFS=$'\t' read -r key version; do
    if [ -n "$key" ]; then
        echo "  Deleting: $key (version: $version)"
        aws s3api delete-object \
            --bucket "$BUCKET_NAME" \
            --key "$key" \
            --version-id "$version" >/dev/null
    fi
done

# Delete delete markers
aws s3api list-object-versions \
    --bucket "$BUCKET_NAME" \
    --output json \
    --query '{Objects: DeleteMarkers[].{Key:Key,VersionId:VersionId}}' | \
jq -r '.Objects[]? | "\(.Key)\t\(.VersionId)"' | \
while IFS=$'\t' read -r key version; do
    if [ -n "$key" ]; then
        echo "  Deleting marker: $key (version: $version)"
        aws s3api delete-object \
            --bucket "$BUCKET_NAME" \
            --key "$key" \
            --version-id "$version" >/dev/null
    fi
done

echo "✓ Bucket emptied: $BUCKET_NAME"
