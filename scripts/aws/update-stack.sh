#!/bin/bash
# Complete stack update workflow
# Integrates: Lambda handlers → template → stack update

set -e

cd "$(dirname "$0")/../.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

TEMPLATE_FILE="infra/stack.yaml"
STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
HANDLERS_DIR="infra/handlers"
BUILD_DIR=".build/handlers"

usage() {
    echo "Usage: $0 <environment> [options]"
    echo ""
    echo "Complete stack update workflow:"
    echo "  1. Package and upload Lambda handlers"
    echo "  2. Upload CloudFormation template"
    echo "  3. Update CloudFormation stack"
    echo "  4. Wait for completion"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options (override current values):"
    echo "  --github-owner <owner>    - Update GitHub repository owner"
    echo "  --github-repo <repo>      - Update GitHub repository name"
    echo "  --github-branch <branch>  - Update GitHub branch"
    echo ""
    echo "Note: Most parameters (instance type, VPC, etc.) use previous values."
    echo "      To change those, use CloudFormation console or AWS CLI directly."
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

get_bucket_name() {
    local env=$1
    local account_id=$(aws sts get-caller-identity --query Account --output text)
    echo "${env}-${STACK_PREFIX}-stack-artifacts-${account_id}"
}

check_bucket_exists() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")

    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo ""
        echo "Error: Stack-artifacts bucket does not exist: $bucket_name"
        echo "This shouldn't happen for an existing stack."
        echo ""
        exit 1
    fi

    echo "  ✓ Bucket exists: $bucket_name"
}

package_and_upload_handlers() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")

    echo ""
    echo "=== Lambda Handlers ==="

    # Create build directory
    rm -rf "$BUILD_DIR"
    mkdir -p "$BUILD_DIR"

    # Package each handler
    for handler_file in app_error_notifier.py pipeline_notifier.py; do
        if [ ! -f "${HANDLERS_DIR}/${handler_file}" ]; then
            echo "Error: Handler not found: ${HANDLERS_DIR}/${handler_file}"
            exit 1
        fi

        local handler_name=$(basename "$handler_file" .py)
        local zip_file="${handler_name}.zip"
        local build_handler_dir="${BUILD_DIR}/${handler_name}"

        echo "Packaging: $handler_name"

        # Create clean build directory for this handler
        rm -rf "$build_handler_dir"
        mkdir -p "$build_handler_dir"

        # Copy handler file and create zip
        cp "${HANDLERS_DIR}/${handler_file}" "${build_handler_dir}/"
        cd "$build_handler_dir"
        zip -q "$zip_file" "${handler_file}"
        cd - > /dev/null

        # Move zip to build root
        mv "${build_handler_dir}/${zip_file}" "${BUILD_DIR}/"

        # Upload to S3
        aws s3 cp "${BUILD_DIR}/${zip_file}" "s3://${bucket_name}/${zip_file}" --only-show-errors

        # Get version ID
        local version_id=$(aws s3api list-object-versions \
            --bucket "$bucket_name" \
            --prefix "$zip_file" \
            --query 'Versions[?IsLatest==`true`].VersionId' \
            --output text)

        echo "  ✓ Uploaded: s3://${bucket_name}/${zip_file} (v${version_id})"
    done
}

upload_template() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")
    local template_key="stack.yaml"

    echo ""
    echo "=== CloudFormation Template ==="
    echo "Uploading template to S3..."

    # Upload template
    aws s3 cp "$TEMPLATE_FILE" "s3://${bucket_name}/${template_key}" --only-show-errors

    # Get version ID
    local version_id=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --prefix "$template_key" \
        --query 'Versions[?IsLatest==`true`].VersionId' \
        --output text)

    # Construct S3 URL
    local template_url="https://${bucket_name}.s3.amazonaws.com/${template_key}"

    echo "  ✓ Template uploaded (v${version_id})"

    # Return the template URL
    echo "$template_url"
}

update_stack() {
    local env=$1
    local template_url=$2
    shift 2
    local stack_name=$(get_stack_name $env)

    echo ""
    echo "=== CloudFormation Stack ==="

    # Slack webhooks for notifications (always update these)
    local slack_pipeline_webhook="${SLACK_PIPELINE_WEBHOOK_URL:-}"
    local slack_app_webhook="${SLACK_APP_WEBHOOK_URL:-}"

    # Parse command-line parameter overrides
    local param_overrides=()
    while [[ $# -gt 0 ]]; do
        case $1 in
            --github-owner)
                param_overrides+=("ParameterKey=GitHubOwner,ParameterValue=$2")
                shift 2
                ;;
            --github-repo)
                param_overrides+=("ParameterKey=GitHubRepo,ParameterValue=$2")
                shift 2
                ;;
            --github-branch)
                param_overrides+=("ParameterKey=GitHubBranch,ParameterValue=$2")
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                echo "Supported update options: --github-owner, --github-repo, --github-branch"
                exit 1
                ;;
        esac
    done

    echo "Updating stack: $stack_name"

    # Build parameters list (use previous values by default, override if specified)
    local params=(
        "ParameterKey=Environment,UsePreviousValue=true"
        "ParameterKey=InstanceType,UsePreviousValue=true"
        "ParameterKey=ErlangVersion,UsePreviousValue=true"
        "ParameterKey=VpcId,UsePreviousValue=true"
        "ParameterKey=ALBSubnets,UsePreviousValue=true"
        "ParameterKey=GitHubOwner,UsePreviousValue=true"
        "ParameterKey=GitHubRepo,UsePreviousValue=true"
        "ParameterKey=GitHubBranch,UsePreviousValue=true"
        "ParameterKey=SlackPipelineWebhookUrl,ParameterValue=$slack_pipeline_webhook"
        "ParameterKey=SlackAppWebhookUrl,ParameterValue=$slack_app_webhook"
    )

    # Replace with overrides if provided
    if [ ${#param_overrides[@]} -gt 0 ]; then
        echo ""
        echo "Parameter overrides:"
        for override in "${param_overrides[@]}"; do
            local key=$(echo "$override" | cut -d',' -f1 | cut -d'=' -f2)
            local value=$(echo "$override" | cut -d',' -f2 | cut -d'=' -f2)
            echo "  $key: $value"
            # Remove the default entry for this key
            params=("${params[@]/ParameterKey=$key,UsePreviousValue=true/}")
            # Add the override
            params+=("$override")
        done
    fi

    echo ""
    echo "Using template: $template_url"
    echo ""

    aws cloudformation update-stack \
        --stack-name "$stack_name" \
        --template-url "$template_url" \
        --parameters "${params[@]}" \
        --capabilities CAPABILITY_NAMED_IAM

    echo ""
    echo "Stack update initiated. Waiting for completion..."
    echo ""

    aws cloudformation wait stack-update-complete \
        --stack-name "$stack_name"

    echo ""
    echo "✓ Stack updated successfully!"
}

# Main execution
if [ $# -lt 1 ]; then
    usage
fi

ENV=$1
shift

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
echo " Stack Update: $ENV"
echo "========================================"
echo ""

echo "Step 1: Checking prerequisites..."
check_bucket_exists "$ENV"

echo ""
echo "Step 2: Packaging and uploading Lambda handlers..."
package_and_upload_handlers "$ENV"

echo ""
echo "Step 3: Uploading CloudFormation template..."
TEMPLATE_URL=$(upload_template "$ENV")

echo ""
echo "Step 4: Updating CloudFormation stack..."
update_stack "$ENV" "$TEMPLATE_URL" "$@"

echo ""
echo "========================================"
echo " ✓ Update Complete"
echo "========================================"
