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
    echo "Options for create/update:"
    echo "  --key-name <name>         - EC2 key pair name (required for create)"
    echo "  --instance-type <type>    - EC2 instance type (default: t3.small)"
    echo "  --ssh-location <cidr>     - SSH access CIDR (default: 0.0.0.0/0)"
    echo "  --subnets <subnet-ids>    - Comma-separated subnet IDs for ALB (required for create)"
    echo "                              Must be at least 2 subnets in different AZs"
    echo ""
    echo "Note: To get default VPC subnet IDs, run:"
    echo "  aws ec2 describe-subnets --filters \"Name=default-for-az,Values=true\" \\"
    echo "    --query 'Subnets[*].[SubnetId,AvailabilityZone]' --output table"
    exit 1
}

get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

create_stack() {
    local env=$1
    shift
    local stack_name=$(get_stack_name $env)

    # Parse options
    local key_name=""
    local instance_type="t3.small"
    local ssh_location="0.0.0.0/0"
    local subnets=""

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
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    if [ -z "$key_name" ]; then
        echo "Error: --key-name is required for creating a stack"
        echo ""
        echo "Available key pairs:"
        aws ec2 describe-key-pairs --query 'KeyPairs[*].KeyName' --output table
        exit 1
    fi

    if [ -z "$subnets" ]; then
        echo "Error: --subnets is required for creating a stack"
        echo ""
        echo "To get default VPC subnet IDs, run:"
        echo "  aws ec2 describe-subnets --filters \"Name=default-for-az,Values=true\" \\"
        echo "    --query 'Subnets[*].[SubnetId,AvailabilityZone]' --output table"
        echo ""
        echo "Then use: --subnets subnet-xxxxx,subnet-yyyyy"
        exit 1
    fi

    echo "Creating stack: $stack_name"
    echo "  Environment: $env"
    echo "  Key Name: $key_name"
    echo "  Instance Type: $instance_type"
    echo "  SSH Location: $ssh_location"
    echo "  ALB Subnets: $subnets"
    echo ""

    aws cloudformation create-stack \
        --stack-name "$stack_name" \
        --template-body "file://$TEMPLATE_FILE" \
        --parameters \
            "ParameterKey=Environment,ParameterValue=$env" \
            "ParameterKey=KeyName,ParameterValue=$key_name" \
            "ParameterKey=InstanceType,ParameterValue=$instance_type" \
            "ParameterKey=SSHLocation,ParameterValue=$ssh_location" \
            "ParameterKey=ALBSubnets,ParameterValue=\"$subnets\"" \
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
