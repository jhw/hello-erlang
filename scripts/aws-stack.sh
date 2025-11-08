#!/bin/bash
# CloudFormation stack management

set -e

cd "$(dirname "$0")/.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

TEMPLATE_FILE="config/stack.yaml"
STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 {deploy|delete|update|status|outputs|events|resources|list} <environment> [options]"
    echo ""
    echo "Commands:"
    echo "  deploy <env>              - Deploy/create a new stack"
    echo "  delete <env>              - Delete a stack"
    echo "  update <env>              - Update an existing stack"
    echo "  status <env>              - Show stack status"
    echo "  outputs <env>             - Show stack outputs"
    echo "  events <env>              - Show recent stack events"
    echo "  resources <env>           - Show stack resources"
    echo "  list                      - List all hello-erlang stacks"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options for deploy:"
    echo "  --instance-type <type>    - EC2 instance type (default: t3.micro)"
    echo "  --subnets <subnet-ids>    - Comma-separated subnet IDs for ALB (optional, auto-discovers)"
    echo ""
    echo "Auto-discovery:"
    echo "  - Subnets: Uses default VPC subnets (at least 2 in different AZs)"
    echo "  - Set DEFAULT_* variables in config/env.sh to override"
    echo ""
    echo "Note: SSH access is not configured. Use AWS SSM Session Manager for emergency access:"
    echo "  aws ssm start-session --target <instance-id>"
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
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

    echo "  Found $subnet_count subnets, using first 2" >&2
    # Return both VPC ID and subnet list separated by |
    echo "${vpc_id}|${subnet_list}"
}

create_stack() {
    local env=$1
    shift
    local stack_name=$(get_stack_name $env)

    # Apply defaults from config/aws.sh if set, otherwise use hardcoded defaults
    local instance_type="${DEFAULT_INSTANCE_TYPE:-t3.micro}"
    local subnets="${DEFAULT_ALB_SUBNETS:-}"
    local vpc_id="${DEFAULT_VPC_ID:-}"

    # Parse command-line options (these override defaults)
    while [[ $# -gt 0 ]]; do
        case $1 in
            --instance-type)
                instance_type="$2"
                shift 2
                ;;
            --subnets)
                subnets="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    # Auto-discover VPC and subnets if not provided
    if [ -z "$subnets" ]; then
        # Need to discover both VPC and subnets
        local vpc_subnet_result
        if ! vpc_subnet_result=$(auto_discover_vpc_and_subnets); then
            echo ""
            echo "Failed to auto-discover VPC/subnets. Please specify manually"
            exit 1
        fi
        # Parse the result (format: vpc_id|subnet_list)
        vpc_id=$(echo "$vpc_subnet_result" | cut -d'|' -f1)
        subnets=$(echo "$vpc_subnet_result" | cut -d'|' -f2)
    elif [ -z "$vpc_id" ]; then
        # Subnets provided but VPC not - look up VPC from first subnet
        local first_subnet=$(echo "$subnets" | cut -d',' -f1)
        vpc_id=$(aws ec2 describe-subnets \
            --subnet-ids "$first_subnet" \
            --query 'Subnets[0].VpcId' \
            --output text 2>/dev/null)

        if [ -z "$vpc_id" ] || [ "$vpc_id" == "None" ]; then
            echo "Error: Could not determine VPC ID from subnet $first_subnet"
            exit 1
        fi
        echo "Discovered VPC ID from subnets: $vpc_id"
    fi

    echo "Creating stack: $stack_name"
    echo "  Environment: $env"
    echo "  Instance Type: $instance_type"
    echo "  VPC ID: $vpc_id"
    echo "  ALB Subnets: $subnets"
    echo "  SSH Access: Disabled (use SSM Session Manager for emergency access)"
    echo ""

    # Build parameters array
    local params=(
        "ParameterKey=Environment,ParameterValue=$env"
        "ParameterKey=InstanceType,ParameterValue=$instance_type"
        "ParameterKey=VpcId,ParameterValue=$vpc_id"
        "ParameterKey=ALBSubnets,ParameterValue=\"$subnets\""
    )

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
            "ParameterKey=InstanceType,UsePreviousValue=true" \
            "ParameterKey=VpcId,UsePreviousValue=true" \
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

show_resources() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    echo "Resources for: $stack_name"
    echo ""

    aws cloudformation describe-stack-resources \
        --stack-name "$stack_name" \
        --query 'StackResources[*].[LogicalResourceId,ResourceType,ResourceStatus,PhysicalResourceId]' \
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
    deploy)
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
    resources)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        show_resources "$2"
        ;;
    list)
        list_stacks
        ;;
    *)
        usage
        ;;
esac
