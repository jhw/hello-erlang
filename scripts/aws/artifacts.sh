#!/bin/bash
# Stack artifacts bucket management
# Manages S3 bucket for Lambda deployment packages

set -e

cd "$(dirname "$0")/../.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)

usage() {
    echo "Usage: $0 {create|delete|empty|list|upload} <environment> [options]"
    echo ""
    echo "Commands:"
    echo "  create <env>              - Create stack-artifacts bucket for environment"
    echo "  delete <env>              - Delete stack-artifacts bucket"
    echo "  empty <env>               - Empty all objects from bucket (keeps bucket)"
    echo "  list <env>                - List all objects in bucket"
    echo "  upload <env> <file>       - Upload a file to bucket (returns S3 URL and version)"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Examples:"
    echo "  $0 create dev             - Create dev-hello-erlang-stack-artifacts-123456789012"
    echo "  $0 upload dev lambda.zip  - Upload and return S3 URI with version"
    echo "  $0 list dev               - Show all artifacts with versions"
    echo "  $0 empty dev              - Remove all artifacts but keep bucket"
    echo "  $0 delete dev             - Delete bucket completely"
    exit 1
}

get_bucket_name() {
    local env=$1
    echo "${env}-${STACK_PREFIX}-stack-artifacts-${ACCOUNT_ID}"
}

create_bucket() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")

    echo "Creating stack artifacts bucket: $bucket_name"

    # Check if bucket already exists
    if aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "Bucket already exists: $bucket_name"
        return 0
    fi

    # Get AWS region
    local region=$(aws configure get region)
    if [ -z "$region" ]; then
        region="us-east-1"
    fi

    # Create bucket
    if [ "$region" == "us-east-1" ]; then
        # us-east-1 doesn't require LocationConstraint
        aws s3api create-bucket \
            --bucket "$bucket_name" \
            --region "$region"
    else
        aws s3api create-bucket \
            --bucket "$bucket_name" \
            --region "$region" \
            --create-bucket-configuration LocationConstraint="$region"
    fi

    # Enable versioning
    echo "Enabling versioning on bucket..."
    aws s3api put-bucket-versioning \
        --bucket "$bucket_name" \
        --versioning-configuration Status=Enabled

    # Block public access
    echo "Configuring public access block..."
    aws s3api put-public-access-block \
        --bucket "$bucket_name" \
        --public-access-block-configuration \
        "BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true"

    # Add lifecycle policy (optional - 90 day retention for stack artifacts)
    echo "Configuring lifecycle policy..."
    aws s3api put-bucket-lifecycle-configuration \
        --bucket "$bucket_name" \
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
        --bucket "$bucket_name" \
        --tagging "TagSet=[{Key=Environment,Value=$env},{Key=Application,Value=hello-erlang},{Key=Purpose,Value=stack-artifacts}]"

    echo "✓ Bucket created successfully: $bucket_name"
    echo "  - Versioning: Enabled"
    echo "  - Public Access: Blocked"
    echo "  - Lifecycle: 90-day retention for old versions"
}

delete_bucket() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")

    echo "Deleting stack artifacts bucket: $bucket_name"

    # Check if bucket exists
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "Bucket does not exist: $bucket_name"
        return 0
    fi

    # Empty bucket first (including all versions)
    echo "Emptying bucket (this may take a moment)..."
    empty_bucket "$env"

    # Delete bucket
    echo "Deleting bucket..."
    aws s3api delete-bucket --bucket "$bucket_name"

    echo "✓ Bucket deleted successfully: $bucket_name"
}

empty_bucket() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")

    echo "Emptying bucket: $bucket_name"

    # Check if bucket exists
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "Bucket does not exist: $bucket_name"
        return 0
    fi

    # Delete all object versions and delete markers
    aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --output json \
        --query '{Objects: Versions[].{Key:Key,VersionId:VersionId}}' | \
    jq -r '.Objects[]? | "\(.Key)\t\(.VersionId)"' | \
    while IFS=$'\t' read -r key version; do
        if [ -n "$key" ]; then
            echo "  Deleting: $key (version: $version)"
            aws s3api delete-object \
                --bucket "$bucket_name" \
                --key "$key" \
                --version-id "$version" >/dev/null
        fi
    done

    # Delete delete markers
    aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --output json \
        --query '{Objects: DeleteMarkers[].{Key:Key,VersionId:VersionId}}' | \
    jq -r '.Objects[]? | "\(.Key)\t\(.VersionId)"' | \
    while IFS=$'\t' read -r key version; do
        if [ -n "$key" ]; then
            echo "  Deleting marker: $key (version: $version)"
            aws s3api delete-object \
                --bucket "$bucket_name" \
                --key "$key" \
                --version-id "$version" >/dev/null
        fi
    done

    echo "✓ Bucket emptied: $bucket_name"
}

list_bucket() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")

    echo "Listing objects in: $bucket_name"
    echo ""

    # Check if bucket exists
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "Bucket does not exist: $bucket_name"
        return 1
    fi

    # List current versions
    aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --query 'Versions[?IsLatest==`true`].[Key,VersionId,LastModified,Size]' \
        --output table

    echo ""
    echo "Total objects: $(aws s3 ls s3://$bucket_name --recursive | wc -l | tr -d ' ')"
}

upload_file() {
    local env=$1
    local file=$2
    local bucket_name=$(get_bucket_name "$env")

    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file"
        return 1
    fi

    # Check if bucket exists
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "Error: Bucket does not exist: $bucket_name"
        echo "Run: $0 create $env"
        return 1
    fi

    local filename=$(basename "$file")
    local s3_key="$filename"

    echo "Uploading: $file"
    echo "       to: s3://$bucket_name/$s3_key"

    # Upload file
    aws s3 cp "$file" "s3://$bucket_name/$s3_key"

    # Get version ID
    local version_id=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --prefix "$s3_key" \
        --query 'Versions[?IsLatest==`true`].VersionId' \
        --output text)

    echo ""
    echo "✓ Upload successful"
    echo "  S3 URI:     s3://$bucket_name/$s3_key"
    echo "  Version ID: $version_id"
    echo ""
    echo "CloudFormation S3 reference:"
    echo "  S3Bucket: $bucket_name"
    echo "  S3Key: $s3_key"
    echo "  S3ObjectVersion: $version_id"
}

# Main command handling
if [ $# -lt 2 ]; then
    usage
fi

COMMAND=$1
ENV=$2

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        echo "Must be: dev, staging, or prod"
        exit 1
        ;;
esac

case "$COMMAND" in
    create)
        create_bucket "$ENV"
        ;;
    delete)
        delete_bucket "$ENV"
        ;;
    empty)
        empty_bucket "$ENV"
        ;;
    list)
        list_bucket "$ENV"
        ;;
    upload)
        if [ $# -lt 3 ]; then
            echo "Error: upload requires a file argument"
            echo "Usage: $0 upload <environment> <file>"
            exit 1
        fi
        upload_file "$ENV" "$3"
        ;;
    *)
        echo "Error: Unknown command: $COMMAND"
        usage
        ;;
esac
