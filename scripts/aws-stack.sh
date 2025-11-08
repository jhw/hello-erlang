#!/bin/bash
# CloudFormation stack management

set -e

cd "$(dirname "$0")/../.."

# Load local AWS config if it exists (gitignored)
if [ -f "config/aws.sh" ]; then
    source "config/aws.sh"
fi

TEMPLATE_FILE="config/ec2-stack.yaml"
STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 {create|delete|update|status|outputs|events|list} <environment> [options]"
    echo ""
    echo "Commands:"
    echo "  create <env>              - Create a new stack"
    echo "  delete <env>              - Delete a stack"
    echo "  update <env>              - Update an existing stack"
    echo "  status <env>              - Show stack status"
    echo "  outputs <env>             - Show stack outputs"
    echo "  events <env>              - Show recent stack events"
    echo "  list                      - List all hello-erlang stacks"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options for create:"
    echo "  --key-name <name>         - EC2 key pair name (optional, auto-discovers first available)"
    echo "  --instance-type <type>    - EC2 instance type (default: t3.small)"
    echo "  --ssh-location <cidr>     - SSH access CIDR (default: 0.0.0.0/0)"
    echo "  --subnets <subnet-ids>    - Comma-separated subnet IDs for ALB (optional, auto-discovers)"
    echo "  --no-key                  - Skip SSH key pair (no SSH access)"
    echo ""
    echo "Auto-discovery:"
    echo "  - Key pair: Uses first available key pair in your account"
    echo "  - Subnets: Uses default VPC subnets (at least 2 in different AZs)"
    echo "  - Set DEFAULT_* variables in config/aws.sh to override"
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

auto_discover_key_pair() {
    echo "Auto-discovering key pair..." >&2
    local key_name=$(aws ec2 describe-key-pairs \
        --query 'KeyPairs[0].KeyName' \
        --output text 2>/dev/null)

    if [ "$key_name" == "None" ] || [ -z "$key_name" ]; then
        echo "" >&2
        echo "Warning: No key pairs found in your AWS account" >&2
        echo "Stack will be created without SSH access (SSM Session Manager only)" >&2
        echo ""
        return 1
    fi

    echo "  Found key pair: $key_name" >&2
    echo "$key_name"
}

auto_discover_subnets() {
    echo "Auto-discovering subnets..." >&2

    # Get default VPC ID
    local vpc_id=$(aws ec2 describe-vpcs \
        --filters "Name=is-default,Values=true" \
        --query 'Vpcs[0].VpcId' \
        --output text 2>/dev/null)

    if [ "$vpc_id" == "None" ] || [ -z "$vpc_id" ]; then
        echo "" >&2
        echo "Error: No default VPC found" >&2
        echo "Please specify subnets manually with --subnets or create a default VPC" >&2
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

    echo "  Found $subnet_count subnets, using first 2" >&2
    echo "$subnet_list"
}

create_stack() {
    local env=$1
    shift
    local stack_name=$(get_stack_name $env)

    # Apply defaults from config/aws.sh if set, otherwise use hardcoded defaults
    local key_name="${DEFAULT_KEY_NAME:-}"
    local instance_type="${DEFAULT_INSTANCE_TYPE:-t3.small}"
    local ssh_location="${DEFAULT_SSH_LOCATION:-0.0.0.0/0}"
    local subnets="${DEFAULT_ALB_SUBNETS:-}"
    local no_key=false

    # Parse command-line options (these override defaults)
    while [[ $# -gt 0 ]]; do
        case $1 in
            --key-name)
                key_name="$2"
                shift 2
                ;;
            --instance-type)
                instance_type="$2"
                shift 2
                ;;
            --ssh-location)
                ssh_location="$2"
                shift 2
                ;;
            --subnets)
                subnets="$2"
                shift 2
                ;;
            --no-key)
                no_key=true
                key_name=""
                shift
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    # Auto-discover key pair if not provided and not explicitly disabled
    if [ -z "$key_name" ] && [ "$no_key" = false ]; then
        if ! key_name=$(auto_discover_key_pair); then
            key_name=""
        fi
    fi

    # Auto-discover subnets if not provided
    if [ -z "$subnets" ]; then
        if ! subnets=$(auto_discover_subnets); then
            echo ""
            echo "Failed to auto-discover subnets. Please specify manually with --subnets"
            exit 1
        fi
    fi

    echo "Creating stack: $stack_name"
    echo "  Environment: $env"
    if [ -z "$key_name" ]; then
        echo "  Key Name: (none - SSM Session Manager only)"
    else
        echo "  Key Name: $key_name"
    fi
    echo "  Instance Type: $instance_type"
    echo "  SSH Location: $ssh_location"
    echo "  ALB Subnets: $subnets"
    echo ""

    # Build parameters array
    local params=(
        "ParameterKey=Environment,ParameterValue=$env"
        "ParameterKey=InstanceType,ParameterValue=$instance_type"
        "ParameterKey=SSHLocation,ParameterValue=$ssh_location"
        "ParameterKey=ALBSubnets,ParameterValue=\"$subnets\""
    )

    # Add KeyName parameter only if provided
    if [ -n "$key_name" ]; then
        params+=("ParameterKey=KeyName,ParameterValue=$key_name")
    else
        params+=("ParameterKey=KeyName,ParameterValue=")
    fi

    aws cloudformation create-stack \
        --stack-name "$stack_name" \
        --template-body "file://$TEMPLATE_FILE" \
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
    show_outputs "$env"
}

delete_stack() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    echo "Deleting stack: $stack_name"
    echo ""
    read -p "Are you sure you want to delete this stack? (yes/no): " confirm

    if [ "$confirm" != "yes" ]; then
        echo "Deletion cancelled"
        exit 0
    fi

    aws cloudformation delete-stack \
        --stack-name "$stack_name"

    echo ""
    echo "Stack deletion initiated. Waiting for completion..."
    echo ""

    aws cloudformation wait stack-delete-complete \
        --stack-name "$stack_name"

    echo ""
    echo "✓ Stack deleted successfully!"
}

update_stack() {
    local env=$1
    shift
    local stack_name=$(get_stack_name $env)

    # Get current parameters
    local current_params=$(aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query 'Stacks[0].Parameters' \
        --output json)

    echo "Updating stack: $stack_name"
    echo ""

    # For simplicity, use existing parameters
    # Could be enhanced to accept parameter overrides
    aws cloudformation update-stack \
        --stack-name "$stack_name" \
        --template-body "file://$TEMPLATE_FILE" \
        --parameters \
            "ParameterKey=Environment,UsePreviousValue=true" \
            "ParameterKey=KeyName,UsePreviousValue=true" \
            "ParameterKey=InstanceType,UsePreviousValue=true" \
            "ParameterKey=SSHLocation,UsePreviousValue=true" \
            "ParameterKey=ALBSubnets,UsePreviousValue=true" \
        --capabilities CAPABILITY_NAMED_IAM

    echo ""
    echo "Stack update initiated. Waiting for completion..."
    echo ""

    aws cloudformation wait stack-update-complete \
        --stack-name "$stack_name"

    echo ""
    echo "✓ Stack updated successfully!"
}

show_status() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query 'Stacks[0].[StackName,StackStatus,CreationTime]' \
        --output table
}

show_outputs() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    echo "Stack outputs for: $stack_name"
    echo ""

    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query 'Stacks[0].Outputs[*].[OutputKey,OutputValue,Description]' \
        --output table
}

show_events() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    echo "Recent events for: $stack_name"
    echo ""

    aws cloudformation describe-stack-events \
        --stack-name "$stack_name" \
        --max-items 20 \
        --query 'StackEvents[*].[Timestamp,ResourceStatus,ResourceType,LogicalResourceId,ResourceStatusReason]' \
        --output table
}

list_stacks() {
    echo "Hello Erlang CloudFormation Stacks:"
    echo ""

    aws cloudformation list-stacks \
        --stack-status-filter CREATE_COMPLETE UPDATE_COMPLETE UPDATE_ROLLBACK_COMPLETE \
        --query "StackSummaries[?starts_with(StackName, '${STACK_PREFIX}')].[StackName,StackStatus,CreationTime]" \
        --output table
}

# Main command router
case "$1" in
    create)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        create_stack "$2" "${@:3}"
        ;;
    delete)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        delete_stack "$2"
        ;;
    update)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        update_stack "$2" "${@:3}"
        ;;
    status)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        show_status "$2"
        ;;
    outputs)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        show_outputs "$2"
        ;;
    events)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        show_events "$2"
        ;;
    list)
        list_stacks
        ;;
    *)
        usage
        ;;
esac
