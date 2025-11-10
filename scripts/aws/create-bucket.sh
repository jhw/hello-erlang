#!/bin/bash
# Create stack-artifacts S3 bucket

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
    echo "Create stack-artifacts S3 bucket for Lambda and template storage."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "The bucket will be created with:"
    echo "  - S3 versioning enabled"
    echo "  - Public access blocked"
    echo "  - 90-day lifecycle for old versions"
    echo "  - Environment tags"
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

echo "Creating stack artifacts bucket: $BUCKET_NAME"

# Check if bucket already exists
if aws s3api head-bucket --bucket "$BUCKET_NAME" 2>/dev/null; then
    echo "Bucket already exists: $BUCKET_NAME"
    exit 0
fi

# Get AWS region
REGION=$(aws configure get region)
if [ -z "$REGION" ]; then
    REGION="us-east-1"
fi

# Create bucket
if [ "$REGION" == "us-east-1" ]; then
    # us-east-1 doesn't require LocationConstraint
    aws s3api create-bucket \
        --bucket "$BUCKET_NAME" \
        --region "$REGION"
else
    aws s3api create-bucket \
        --bucket "$BUCKET_NAME" \
        --region "$REGION" \
        --create-bucket-configuration LocationConstraint="$REGION"
fi

# Enable versioning
echo "Enabling versioning on bucket..."
aws s3api put-bucket-versioning \
    --bucket "$BUCKET_NAME" \
    --versioning-configuration Status=Enabled

# Block public access
echo "Configuring public access block..."
aws s3api put-public-access-block \
    --bucket "$BUCKET_NAME" \
    --public-access-block-configuration \
    "BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true"

# Add lifecycle policy
echo "Configuring lifecycle policy..."
aws s3api put-bucket-lifecycle-configuration \
    --bucket "$BUCKET_NAME" \
    --lifecycle-configuration '{
      "Rules": [
        {
          "Id": "DeleteOldVersions",
          "Status": "Enabled",
          "NoncurrentVersionExpiration": {
            "NoncurrentDays": 90
          }
        }
      ]
    }'

# Add tags
aws s3api put-bucket-tagging \
    --bucket "$BUCKET_NAME" \
    --tagging "TagSet=[{Key=Environment,Value=$ENV},{Key=Application,Value=hello-erlang},{Key=Purpose,Value=stack-artifacts}]"

echo "✓ Bucket created successfully: $BUCKET_NAME"
echo "  - Versioning: Enabled"
echo "  - Public Access: Blocked"
echo "  - Lifecycle: 90-day retention for old versions"
