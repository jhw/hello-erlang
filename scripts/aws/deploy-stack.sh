#!/bin/bash
# Complete stack deployment workflow
# Integrates: bucket check → Lambda handlers → template → stack creation

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
    echo "Complete stack deployment workflow:"
    echo "  1. Verify stack-artifacts bucket exists"
    echo "  2. Package and upload Lambda handlers"
    echo "  3. Upload CloudFormation template"
    echo "  4. Create CloudFormation stack"
    echo "  5. Wait for completion and show outputs"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Prerequisites:"
    echo "  - Stack-artifacts bucket must exist: ./scripts/aws/create-bucket.sh <env>"
    echo ""
    echo "Options:"
    echo "  --instance-type <type>    - EC2 instance type (default: t3.micro)"
    echo "  --erlang-version <ver>    - Erlang/OTP version (default: 27.1)"
    echo "  --subnets <subnet-ids>    - Comma-separated subnet IDs for ALB (optional, auto-discovers)"
    echo "  --github-owner <owner>    - GitHub repository owner (required, or set GITHUB_OWNER in config/env.sh)"
    echo "  --github-repo <repo>      - GitHub repository name (default: hello-erlang)"
    echo "  --github-branch <branch>  - GitHub branch to monitor (default: deploy/<env>)"
    echo ""
    echo "Auto-discovery:"
    echo "  - Subnets: Uses default VPC subnets (at least 2 in different AZs)"
    echo "  - Set DEFAULT_* variables in config/env.sh to override"
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
        echo "Create it first: ./scripts/aws/create-bucket.sh $env"
        echo ""
        exit 1
    fi

    echo "  ✓ Bucket exists: $bucket_name"
}

package_and_upload_handlers() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")
    local zip_file="handlers.zip"

    echo ""
    echo "=== Lambda Handlers ==="

    # Create build directory
    rm -rf "$BUILD_DIR"
    mkdir -p "$BUILD_DIR"

    # Check all handlers exist
    for handler_file in app_error_notifier.py pipeline_notifier.py; do
        if [ ! -f "${HANDLERS_DIR}/${handler_file}" ]; then
            echo "Error: Handler not found: ${HANDLERS_DIR}/${handler_file}"
            exit 1
        fi
    done

    echo "Packaging all handlers into single zip..."

    # Create single zip with all handlers
    cd "$HANDLERS_DIR"
    zip -q "$zip_file" *.py
    mv "$zip_file" "../../${BUILD_DIR}/"
    cd - > /dev/null

    # Upload to S3
    aws s3 cp "${BUILD_DIR}/${zip_file}" "s3://${bucket_name}/${zip_file}" --only-show-errors

    # Get version ID
    local version_id=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --prefix "$zip_file" \
        --query 'Versions[?IsLatest==`true`].VersionId' \
        --output text)

    echo "  ✓ Uploaded: s3://${bucket_name}/${zip_file} (v${version_id})"
    echo "  Contains: app_error_notifier.py, pipeline_notifier.py"
}

upload_template() {
    local env=$1
    local bucket_name=$(get_bucket_name "$env")
    local template_key="stack.yaml"

    echo "" >&2
    echo "=== CloudFormation Template ===" >&2
    echo "Uploading template to S3..." >&2

    # Upload template
    aws s3 cp "$TEMPLATE_FILE" "s3://${bucket_name}/${template_key}" --only-show-errors

    # Get version ID
    local version_id=$(aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --prefix "$template_key" \
        --query 'Versions[?IsLatest==`true`].VersionId' \
        --output text)

    # Get bucket region
    local region=$(aws s3api get-bucket-location --bucket "$bucket_name" --query 'LocationConstraint' --output text)
    if [ "$region" == "None" ] || [ -z "$region" ]; then
        region="us-east-1"
    fi

    # Construct S3 URL (CloudFormation requires s3.region.amazonaws.com format)
    local template_url="https://s3.${region}.amazonaws.com/${bucket_name}/${template_key}"

    echo "  ✓ Template uploaded (v${version_id})" >&2
    echo "  URL: $template_url" >&2

    # Return the template URL (stdout only)
    echo "$template_url"
}

auto_discover_vpc_and_subnets() {
    echo "Auto-discovering VPC and subnets..." >&2

    # Get default VPC ID
    local vpc_id=$(aws ec2 describe-vpcs \
        --filters "Name=is-default,Values=true" \
        --query 'Vpcs[0].VpcId' \
        --output text 2>/dev/null)

    if [ "$vpc_id" == "None" ] || [ -z "$vpc_id" ]; then
        echo "" >&2
        echo "Error: No default VPC found" >&2
        echo "Please specify VPC and subnets manually or create a default VPC" >&2
        return 1
    fi

    echo "  Found default VPC: $vpc_id" >&2

    # Get subnets in default VPC, sorted by AZ
    local subnets=$(aws ec2 describe-subnets \
        --filters "Name=vpc-id,Values=$vpc_id" \
        --query 'Subnets | sort_by(@, &AvailabilityZone)[*].SubnetId' \
        --output text 2>/dev/null)

    if [ -z "$subnets" ]; then
        echo "" >&2
        echo "Error: No subnets found in default VPC" >&2
        return 1
    fi

    # Convert to comma-separated (take first 2 for different AZs)
    local subnet_list=$(echo "$subnets" | tr '\t' ',' | cut -d',' -f1,2)
    local subnet_count=$(echo "$subnets" | wc -w | tr -d ' ')

    if [ "$subnet_count" -lt 2 ]; then
        echo "" >&2
        echo "Error: At least 2 subnets required, found only $subnet_count" >&2
        return 1
    fi

    echo "  Found subnets: $subnet_list" >&2

    # Output format: vpc_id,subnet_list
    echo "${vpc_id},${subnet_list}"
}

create_stack() {
    local env=$1
    local template_url=$2
    shift 2
    local stack_name=$(get_stack_name $env)

    echo ""
    echo "=== CloudFormation Stack ==="

    # Apply defaults from config/env.sh if set, otherwise use hardcoded defaults
    local instance_type="${DEFAULT_INSTANCE_TYPE:-t3.micro}"
    local erlang_version="${DEFAULT_ERLANG_VERSION:-27.1}"
    local subnets="${DEFAULT_ALB_SUBNETS:-}"
    local vpc_id="${DEFAULT_VPC_ID:-}"

    # GitHub configuration from env.sh
    local github_owner="${GITHUB_OWNER:-}"
    local github_repo="${GITHUB_REPO:-hello-erlang}"
    local github_branch=""

    # Slack webhooks for notifications
    local slack_pipeline_webhook="${SLACK_PIPELINE_WEBHOOK_URL:-}"
    local slack_app_webhook="${SLACK_APP_WEBHOOK_URL:-}"

    # Select branch based on environment
    case "$env" in
        dev)
            github_branch="${DEV_BRANCH:-deploy/dev}"
            ;;
        staging)
            github_branch="${STAGING_BRANCH:-deploy/staging}"
            ;;
        prod)
            github_branch="${PROD_BRANCH:-deploy/prod}"
            ;;
    esac

    # Parse command-line options (these override defaults)
    while [[ $# -gt 0 ]]; do
        case $1 in
            --instance-type)
                instance_type="$2"
                shift 2
                ;;
            --erlang-version)
                erlang_version="$2"
                shift 2
                ;;
            --subnets)
                subnets="$2"
                shift 2
                ;;
            --github-owner)
                github_owner="$2"
                shift 2
                ;;
            --github-repo)
                github_repo="$2"
                shift 2
                ;;
            --github-branch)
                github_branch="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    # Validate GitHub owner
    if [ -z "$github_owner" ]; then
        echo "Error: GitHub owner is required"
        echo "Provide via --github-owner or set GITHUB_OWNER in config/env.sh"
        exit 1
    fi

    # Auto-discover VPC and subnets if not provided
    if [ -z "$subnets" ] || [ -z "$vpc_id" ]; then
        local discovery_result=$(auto_discover_vpc_and_subnets)
        vpc_id=$(echo "$discovery_result" | cut -d',' -f1)
        subnets=$(echo "$discovery_result" | cut -d',' -f2)
    else
        # If subnets provided but no VPC, discover VPC from first subnet
        if [ -z "$vpc_id" ]; then
            local first_subnet=$(echo "$subnets" | cut -d',' -f1)
            vpc_id=$(aws ec2 describe-subnets \
                --subnet-ids "$first_subnet" \
                --query 'Subnets[0].VpcId' \
                --output text)

            if [ -z "$vpc_id" ] || [ "$vpc_id" == "None" ]; then
                echo "Error: Could not determine VPC ID from subnet $first_subnet"
                exit 1
            fi
            echo "Discovered VPC ID from subnets: $vpc_id"
        fi
    fi

    echo "Creating stack: $stack_name"
    echo "  Environment: $env"
    echo "  Instance Type: $instance_type"
    echo "  Erlang Version: $erlang_version"
    echo "  VPC ID: $vpc_id"
    echo "  ALB Subnets: $subnets"
    echo "  GitHub Owner: $github_owner"
    echo "  GitHub Repo: $github_repo"
    echo "  GitHub Branch: $github_branch"
    if [ -n "$slack_pipeline_webhook" ]; then
        echo "  Slack Pipeline Alerts: Enabled"
    else
        echo "  Slack Pipeline Alerts: Disabled (set SLACK_PIPELINE_WEBHOOK_URL in config/env.sh)"
    fi
    if [ -n "$slack_app_webhook" ]; then
        echo "  Slack App Error Alerts: Enabled"
    else
        echo "  Slack App Error Alerts: Disabled (set SLACK_APP_WEBHOOK_URL in config/env.sh)"
    fi
    echo ""

    # Build parameters array
    local params=(
        "ParameterKey=Environment,ParameterValue=$env"
        "ParameterKey=InstanceType,ParameterValue=$instance_type"
        "ParameterKey=ErlangVersion,ParameterValue=$erlang_version"
        "ParameterKey=VpcId,ParameterValue=$vpc_id"
        "ParameterKey=ALBSubnets,ParameterValue=\"$subnets\""
        "ParameterKey=GitHubOwner,ParameterValue=$github_owner"
        "ParameterKey=GitHubRepo,ParameterValue=$github_repo"
        "ParameterKey=GitHubBranch,ParameterValue=$github_branch"
        "ParameterKey=SlackPipelineWebhookUrl,ParameterValue=$slack_pipeline_webhook"
        "ParameterKey=SlackAppWebhookUrl,ParameterValue=$slack_app_webhook"
    )

    aws cloudformation create-stack \
        --stack-name "$stack_name" \
        --template-url "$template_url" \
        --parameters "${params[@]}" \
        --capabilities CAPABILITY_NAMED_IAM \
        --tags \
            "Key=Environment,Value=$env" \
            "Key=Application,Value=hello-erlang" \
            "Key=ManagedBy,Value=CloudFormation"

    echo ""
    echo "Stack creation initiated. Waiting for completion..."
    echo "This may take 3-5 minutes."
    echo ""

    aws cloudformation wait stack-create-complete \
        --stack-name "$stack_name"

    echo ""
    echo "✓ Stack created successfully!"
    echo ""

    # Show outputs
    echo "Stack outputs:"
    echo ""
    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query 'Stacks[0].Outputs' \
        --output table
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
echo " Stack Deployment: $ENV"
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
echo "Step 4: Creating CloudFormation stack..."
create_stack "$ENV" "$TEMPLATE_URL" "$@"

echo ""
echo "========================================"
echo " ✓ Deployment Complete"
echo "========================================"
echo ""
echo "Next steps:"
echo "  - Complete GitHub CodeStar connection: See stack outputs above"
echo "  - Test endpoints: ./scripts/aws/test.sh echo $ENV \"hello\""
echo "  - View logs: ./scripts/aws/debug.sh instance-logs $ENV"
