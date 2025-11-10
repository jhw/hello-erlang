#!/bin/bash
# Delete CloudFormation stack and optionally clean up S3 buckets

set -e

cd "$(dirname "$0")/../.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 <environment> [--delete-buckets]"
    echo ""
    echo "Delete CloudFormation stack."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options:"
    echo "  --delete-buckets    - Also delete S3 buckets after stack deletion"
    echo ""
    echo "Note: Pipeline artifacts bucket will be emptied automatically before"
    echo "      stack deletion (required by CloudFormation). Stack artifacts"
    echo "      bucket is managed separately and not deleted unless you specify"
    echo "      --delete-buckets."
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

get_bucket_name() {
    local env=$1
    local bucket_type=$2
    local account_id=$(aws sts get-caller-identity --query Account --output text)
    echo "${env}-${STACK_PREFIX}-${bucket_type}-${account_id}"
}

empty_pipeline_bucket() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env" "pipeline-artifacts")

    echo "Checking pipeline artifacts bucket: $bucket_name"

    # Check if bucket exists
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "  ✓ Bucket does not exist (already deleted or never created)"
        return 0
    fi

    echo "  ✓ Bucket exists, emptying..."

    # Delete all versions
    local versions=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --output json \
        --query '{Objects: Versions[].{Key:Key,VersionId:VersionId}}' 2>/dev/null)

    if [ "$versions" != "null" ] && [ -n "$versions" ]; then
        echo "$versions" | jq -c '.Objects[]?' 2>/dev/null | while read -r obj; do
            if [ -n "$obj" ]; then
                key=$(echo "$obj" | jq -r '.Key')
                version=$(echo "$obj" | jq -r '.VersionId')
                echo "    Deleting: $key (version: $version)"
                aws s3api delete-object \
                    --bucket "$bucket_name" \
                    --key "$key" \
                    --version-id "$version" >/dev/null 2>&1
            fi
        done
    fi

    # Delete all delete markers
    local markers=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --output json \
        --query '{Objects: DeleteMarkers[].{Key:Key,VersionId:VersionId}}' 2>/dev/null)

    if [ "$markers" != "null" ] && [ -n "$markers" ]; then
        echo "$markers" | jq -c '.Objects[]?' 2>/dev/null | while read -r obj; do
            if [ -n "$obj" ]; then
                key=$(echo "$obj" | jq -r '.Key')
                version=$(echo "$obj" | jq -r '.VersionId')
                echo "    Deleting marker: $key (version: $version)"
                aws s3api delete-object \
                    --bucket "$bucket_name" \
                    --key "$key" \
                    --version-id "$version" >/dev/null 2>&1
            fi
        done
    fi

    echo "  ✓ Pipeline artifacts bucket emptied"
}

delete_buckets() {
    local env=$1

    echo ""
    echo "Deleting S3 buckets..."

    # Delete stack-artifacts bucket
    local stack_bucket=$(get_bucket_name "$env" "stack-artifacts")
    if aws s3api head-bucket --bucket "$stack_bucket" 2>/dev/null; then
        echo "Deleting stack artifacts bucket: $stack_bucket"
        ./scripts/aws/empty-bucket.sh "$env" 2>/dev/null || true
        ./scripts/aws/delete-bucket.sh "$env" 2>/dev/null || true
        echo "  ✓ Stack artifacts bucket deleted"
    else
        echo "  ✓ Stack artifacts bucket does not exist"
    fi

    # Pipeline artifacts bucket should already be deleted by CloudFormation
    local pipeline_bucket=$(get_bucket_name "$env" "pipeline-artifacts")
    if aws s3api head-bucket --bucket "$pipeline_bucket" 2>/dev/null; then
        echo "Warning: Pipeline artifacts bucket still exists: $pipeline_bucket"
        echo "This is managed by CloudFormation and should have been deleted."
    fi
}

delete_stack() {
    local env=$1
    local delete_buckets_flag=$2
    local stack_name=$(get_stack_name $env)

    echo "========================================"
    echo " Stack Deletion: $ENV"
    echo "========================================"
    echo ""
    echo "Stack: $stack_name"
    if [ "$delete_buckets_flag" == "true" ]; then
        echo "S3 Buckets: Will be deleted after stack deletion"
    else
        echo "S3 Buckets: Stack artifacts bucket will be preserved"
    fi
    echo ""

    read -p "Are you sure you want to delete this stack? (yes/no): " confirm

    if [ "$confirm" != "yes" ]; then
        echo "Deletion cancelled"
        exit 0
    fi

    echo ""
    echo "Step 1: Preparing stack for deletion..."
    echo ""

    # Empty pipeline artifacts bucket (required by CloudFormation)
    empty_pipeline_bucket "$env"

    echo ""
    echo "Step 2: Initiating stack deletion..."
    aws cloudformation delete-stack \
        --stack-name "$stack_name"

    echo ""
    echo "Waiting for deletion to complete..."
    echo ""

    aws cloudformation wait stack-delete-complete \
        --stack-name "$stack_name"

    echo ""
    echo "✓ Stack deleted successfully!"

    # Delete buckets if requested
    if [ "$delete_buckets_flag" == "true" ]; then
        delete_buckets "$env"
    else
        echo ""
        echo "Note: Stack artifacts bucket preserved."
        echo "To delete it later: ./scripts/aws/delete-bucket.sh $env"
    fi

    echo ""
    echo "========================================"
    echo " ✓ Deletion Complete"
    echo "========================================"
}

# Main execution
if [ $# -lt 1 ]; then
    usage
fi

ENV=$1
DELETE_BUCKETS=false

# Parse options
shift
while [[ $# -gt 0 ]]; do
    case $1 in
        --delete-buckets)
            DELETE_BUCKETS=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        echo "Must be: dev, staging, or prod"
        exit 1
        ;;
esac

delete_stack "$ENV" "$DELETE_BUCKETS"
