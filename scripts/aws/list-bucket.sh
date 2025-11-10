#!/bin/bash
# List stack-artifacts S3 bucket contents

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
    echo "List stack-artifacts S3 bucket contents."
    echo ""
    echo "Environments: dev, staging, prod"
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

echo "Listing objects in: $BUCKET_NAME"
echo ""

# Check if bucket exists
if ! aws s3api head-bucket --bucket "$BUCKET_NAME" 2>/dev/null; then
    echo "Bucket does not exist: $BUCKET_NAME"
    exit 1
fi

# List current versions
aws s3api list-object-versions \
    --bucket "$BUCKET_NAME" \
    --query 'Versions[?IsLatest==`true`].[Key,VersionId,LastModified,Size]' \
    --output table

echo ""
echo "Total objects: $(aws s3 ls s3://$BUCKET_NAME --recursive | wc -l | tr -d ' ')"
