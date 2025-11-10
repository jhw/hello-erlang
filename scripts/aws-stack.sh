#!/bin/bash
# CloudFormation stack management

set -e

cd "$(dirname "$0")/.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

TEMPLATE_FILE="config/aws/stack.yaml"
STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 {deploy|delete|update|status|outputs|resources|events|restart-agents} <environment> [options]"
    echo ""
    echo "Commands:"
    echo "  deploy <env>              - Deploy/create a new stack"
    echo "  delete <env>              - Delete a stack"
    echo "  update <env>              - Update an existing stack"
    echo "  status <env>              - Show stack status"
    echo "  outputs <env>             - Show stack outputs"
    echo "  resources <env>           - Show stack resources"
    echo "  events <env> [max-items]  - Show stack events (default: 50, useful for debugging)"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options for deploy:"
    echo "  --instance-type <type>    - EC2 instance type (default: t3.micro)"
    echo "  --erlang-version <ver>    - Erlang/OTP version (default: 27.1)"
    echo "  --subnets <subnet-ids>    - Comma-separated subnet IDs for ALB (optional, auto-discovers)"
    echo "  --github-owner <owner>    - GitHub repository owner (required, or set GITHUB_OWNER in env.sh)"
    echo "  --github-repo <repo>      - GitHub repository name (default: hello-erlang)"
    echo "  --github-branch <branch>  - GitHub branch to monitor (default: deploy/<env>)"
    echo ""
    echo "Options for update:"
    echo "  --github-owner <owner>    - Update GitHub repository owner"
    echo "  --github-repo <repo>      - Update GitHub repository name"
    echo "  --github-branch <branch>  - Update GitHub branch"
    echo ""
    echo "Auto-discovery:"
    echo "  - Subnets: Uses default VPC subnets (at least 2 in different AZs)"
    echo "  - Set DEFAULT_* variables in config/env.sh to override"
    echo ""
    echo "GitHub Configuration:"
    echo "  - Set GITHUB_OWNER, GITHUB_REPO, and branch variables in config/env.sh"
    echo "  - After stack creation, complete CodeStar Connection OAuth in AWS Console"
    echo "  - See stack outputs for connection status and activation instructions"
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

    # Validate GitHub configuration
    if [ -z "$github_owner" ]; then
        echo "Error: GITHUB_OWNER not set"
        echo "Please set GITHUB_OWNER in config/env.sh or pass --github-owner"
        exit 1
    fi

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
    echo "  SSH Access: Disabled (use SSM Session Manager for emergency access)"
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

empty_artifact_bucket() {
    local env=$1
    local bucket_name="${env}-hello-erlang-artifacts-$(aws sts get-caller-identity --query Account --output text)"

    echo "Checking artifact bucket: $bucket_name"

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

    echo "  ✓ Bucket emptied"
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

    echo ""
    echo "Preparing stack for deletion..."

    # Empty S3 bucket before attempting deletion
    empty_artifact_bucket "$env"

    echo ""
    echo "Initiating stack deletion..."
    aws cloudformation delete-stack \
        --stack-name "$stack_name"

    echo ""
    echo "Waiting for deletion to complete..."
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

    # Slack webhooks for notifications
    local slack_pipeline_webhook="${SLACK_PIPELINE_WEBHOOK_URL:-}"
    local slack_app_webhook="${SLACK_APP_WEBHOOK_URL:-}"

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
        for override in "${param_overrides[@]}"; do
            local key=$(echo "$override" | cut -d',' -f1 | cut -d'=' -f2)
            # Remove the default entry for this key
            params=("${params[@]/ParameterKey=$key,UsePreviousValue=true/}")
            # Add the override
            params+=("$override")
        done
    fi

    aws cloudformation update-stack \
        --stack-name "$stack_name" \
        --template-body "file://$TEMPLATE_FILE" \
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

show_events() {
    local env=$1
    local max_items=${2:-50}
    local stack_name=$(get_stack_name $env)

    echo "CloudFormation events for: $stack_name"
    echo "Showing last $max_items events"
    echo ""

    # Get events with full details including ResourceStatusReason
    aws cloudformation describe-stack-events \
        --stack-name "$stack_name" \
        --max-items "$max_items" \
        --output json | jq -r '
        .StackEvents[] |
        [
            .Timestamp,
            .ResourceStatus,
            .ResourceType,
            .LogicalResourceId,
            (.ResourceStatusReason // "")
        ] |
        @tsv
    ' | while IFS=$'\t' read -r timestamp status type resource reason; do
        # Color code based on status
        case "$status" in
            *FAILED*|*ROLLBACK*)
                color="\033[31m"  # Red
                ;;
            *COMPLETE*)
                color="\033[32m"  # Green
                ;;
            *IN_PROGRESS*)
                color="\033[33m"  # Yellow
                ;;
            *)
                color="\033[0m"   # Default
                ;;
        esac

        reset="\033[0m"

        # Format output
        printf "${color}%-25s %-20s %-35s %s${reset}\n" "$timestamp" "$status" "$resource" "$type"

        # Show reason on next line if present and it's a failure
        if [ -n "$reason" ] && [[ "$status" == *"FAILED"* ]]; then
            printf "  ${color}└─ Reason: %s${reset}\n" "$reason"
        fi
    done

    echo ""
    echo "Legend:"
    echo -e "  \033[32m■\033[0m Success (COMPLETE)"
    echo -e "  \033[33m■\033[0m In Progress"
    echo -e "  \033[31m■\033[0m Failed or Rollback"
    echo ""
    echo "To see more events: $0 events $env 100"
}

restart_agents() {
    local env=$1
    local stack_name=$(get_stack_name $env)

    echo "Restarting CloudWatch and CodeDeploy agents..."
    echo "  Environment: $env"
    echo ""

    # Get EC2 instance ID from stack
    local instance_id=$(aws cloudformation describe-stack-resources \
        --stack-name "$stack_name" \
        --query 'StackResources[?ResourceType==`AWS::EC2::Instance`].PhysicalResourceId' \
        --output text 2>/dev/null)

    if [ -z "$instance_id" ]; then
        echo "Error: Could not find EC2 instance for environment '$env'" >&2
        exit 1
    fi

    echo "Instance ID: $instance_id"
    echo ""

    # Create updated CloudWatch Agent config
    cat > /tmp/cw-agent-config-$$.json <<EOF
{
  "logs": {
    "logs_collected": {
      "files": {
        "collect_list": [
          {
            "file_path": "/var/log/aws/codedeploy-agent/codedeploy-agent.log",
            "log_group_name": "/aws/codedeploy/$env/agent",
            "log_stream_name": "{instance_id}/codedeploy-agent.log",
            "retention_in_days": 7
          },
          {
            "file_path": "/opt/codedeploy-agent/deployment-root/deployment-logs/codedeploy-agent-deployments.log",
            "log_group_name": "/aws/codedeploy/$env/deployments",
            "log_stream_name": "{instance_id}/deployments.log",
            "retention_in_days": 7
          },
          {
            "file_path": "/tmp/codedeploy-agent.update.log",
            "log_group_name": "/aws/codedeploy/$env/updates",
            "log_stream_name": "{instance_id}/updates.log",
            "retention_in_days": 3
          },
          {
            "file_path": "/var/log/hello_erlang/errors.log",
            "log_group_name": "/aws/ec2/hello-erlang/$env",
            "log_stream_name": "{instance_id}/errors",
            "timezone": "UTC",
            "retention_in_days": 7
          }
        ]
      }
    }
  }
}
EOF

    # Prepare the restart commands
    local config_content=$(cat /tmp/cw-agent-config-$$.json | sed 's/"/\\"/g' | tr '\n' ' ')
    rm /tmp/cw-agent-config-$$.json

    echo "Sending restart command via SSM..."
    local command_id=$(aws ssm send-command \
        --document-name "AWS-RunShellScript" \
        --targets "Key=instanceids,Values=$instance_id" \
        --parameters "commands=[
            \"echo 'Updating CloudWatch Agent config...'\",
            \"cat > /tmp/cw-agent-config.json <<'EOFCONFIG'\",
            \"$config_content\",
            \"EOFCONFIG\",
            \"sudo cp /tmp/cw-agent-config.json /opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json\",
            \"echo 'Restarting CloudWatch Agent...'\",
            \"sudo /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl -a fetch-config -m ec2 -s -c file:/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json\",
            \"echo 'Restarting CodeDeploy Agent...'\",
            \"sudo systemctl restart codedeploy-agent\",
            \"echo 'Done - agents restarted'\",
            \"sudo systemctl status codedeploy-agent | head -5\",
            \"sudo /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl -m ec2 -a query | head -10\"
        ]" \
        --query 'Command.CommandId' \
        --output text 2>/dev/null)

    if [ -z "$command_id" ]; then
        echo "Error: Failed to send restart command" >&2
        exit 1
    fi

    echo "Command ID: $command_id"
    echo "Waiting for command to complete..."
    echo ""

    # Wait for command to complete
    sleep 5

    local status=""
    for i in {1..30}; do
        status=$(aws ssm get-command-invocation \
            --command-id "$command_id" \
            --instance-id "$instance_id" \
            --query 'Status' \
            --output text 2>/dev/null)

        if [ "$status" == "Success" ]; then
            echo "✓ Agents restarted successfully"
            echo ""
            echo "Output:"
            aws ssm get-command-invocation \
                --command-id "$command_id" \
                --instance-id "$instance_id" \
                --query 'StandardOutputContent' \
                --output text 2>/dev/null
            return 0
        elif [ "$status" == "Failed" ]; then
            echo "✗ Agent restart failed"
            echo ""
            echo "Error output:"
            aws ssm get-command-invocation \
                --command-id "$command_id" \
                --instance-id "$instance_id" \
                --query 'StandardErrorContent' \
                --output text 2>/dev/null
            exit 1
        fi

        echo -n "."
        sleep 2
    done

    echo ""
    echo "⚠ Command timed out (status: $status)"
    echo "Check SSM console for details: command-id=$command_id"
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
    resources)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        show_resources "$2"
        ;;
    events)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        show_events "$2" "$3"
        ;;
    restart-agents)
        if [ -z "$2" ]; then
            echo "Error: Environment required"
            usage
        fi
        restart_agents "$2"
        ;;
    *)
        usage
        ;;
esac
