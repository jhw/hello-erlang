#!/bin/bash
# Package and deploy Lambda handlers to S3
# This script packages Python Lambda handlers and uploads them to the stack-artifacts bucket

set -e

cd "$(dirname "$0")/../.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
HANDLERS_DIR="infra/handlers"
BUILD_DIR=".build/handlers"

usage() {
    echo "Usage: $0 <environment>"
    echo ""
    echo "Package and deploy Lambda handlers to stack-artifacts bucket."
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "This script:"
    echo "  1. Creates deployment packages for each Lambda handler"
    echo "  2. Uploads them to the stack-artifacts S3 bucket"
    echo "  3. Reports S3 URIs and version IDs for CloudFormation"
    echo ""
    echo "Prerequisites:"
    echo "  - Stack-artifacts bucket must exist (run: scripts/aws/artifacts.sh create <env>)"
    echo ""
    echo "Handlers packaged:"
    echo "  - app_error_notifier.py → app_error_notifier.zip"
    echo "  - pipeline_notifier.py → pipeline_notifier.zip"
    exit 1
}

package_handler() {
    local handler_file=$1
    local handler_name=$(basename "$handler_file" .py)
    local zip_file="${handler_name}.zip"
    local build_handler_dir="${BUILD_DIR}/${handler_name}"

    echo "Packaging: $handler_name"

    # Create clean build directory for this handler
    rm -rf "$build_handler_dir"
    mkdir -p "$build_handler_dir"

    # Copy handler file to build directory
    cp "${HANDLERS_DIR}/${handler_file}" "${build_handler_dir}/"

    # Create zip in build directory
    cd "$build_handler_dir"
    zip -q "$zip_file" "${handler_file}"
    cd - > /dev/null

    # Move zip to build root
    mv "${build_handler_dir}/${zip_file}" "${BUILD_DIR}/"

    echo "  ✓ Created: ${BUILD_DIR}/${zip_file}"
}

upload_handler() {
    local zip_file=$1
    local env=$2
    local bucket_name="${env}-${STACK_PREFIX}-stack-artifacts-${ACCOUNT_ID}"

    echo ""
    echo "Uploading: $zip_file to $bucket_name"

    # Check if bucket exists
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo ""
        echo "Error: Stack artifacts bucket does not exist: $bucket_name"
        echo "Create it first: scripts/aws/artifacts.sh create $env"
        return 1
    fi

    # Upload to S3
    aws s3 cp "${BUILD_DIR}/${zip_file}" "s3://${bucket_name}/${zip_file}" --only-show-errors

    # Get version ID
    local version_id=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --prefix "$zip_file" \
        --query 'Versions[?IsLatest==`true`].VersionId' \
        --output text)

    echo "  ✓ Uploaded successfully"
    echo "  S3 URI:     s3://${bucket_name}/${zip_file}"
    echo "  Version ID: $version_id"
}

# Main execution
if [ $# -lt 1 ]; then
    usage
fi

ENV=$1

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        echo "Must be: dev, staging, or prod"
        exit 1
        ;;
esac

echo "========================================"
echo " Lambda Handler Deployment"
echo "========================================"
echo "Environment: $ENV"
echo "Handlers directory: $HANDLERS_DIR"
echo "Build directory: $BUILD_DIR"
echo ""

# Create build directory
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# Package all handlers
echo "Step 1: Packaging handlers..."
echo ""

for handler_file in app_error_notifier.py pipeline_notifier.py; do
    if [ ! -f "${HANDLERS_DIR}/${handler_file}" ]; then
        echo "Error: Handler not found: ${HANDLERS_DIR}/${handler_file}"
        exit 1
    fi
    package_handler "$handler_file"
done

echo ""
echo "Step 2: Uploading to S3..."

for zip_file in app_error_notifier.zip pipeline_notifier.zip; do
    upload_handler "$zip_file" "$ENV"
done

echo ""
echo "========================================"
echo " ✓ Deployment Complete"
echo "========================================"
echo ""
echo "Next steps:"
echo "  1. Deploy or update CloudFormation stack:"
echo "     scripts/aws/stack.sh deploy $ENV"
echo "  2. Or if stack exists, update it:"
echo "     scripts/aws/stack.sh update $ENV"
echo ""
echo "The Lambda functions will automatically use the uploaded code."
