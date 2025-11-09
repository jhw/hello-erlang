#!/bin/bash
# Debugging and inspection tools for AWS resources

set -e

cd "$(dirname "$0")/.."

# Load local environment config if it exists (gitignored)
if [ -f "config/env.sh" ]; then
    source "config/env.sh"
fi

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

usage() {
    echo "Usage: $0 {list-builds|list-artifacts|logs|list-deployments|deployment-logs|instance-logs|stack-events|list-stacks|empty-bucket|ping} <environment> [options]"
    echo ""
    echo "CodeBuild Commands:"
    echo "  list-builds <env>               - List recent CodeBuild builds"
    echo "  logs <env> <build-id>           - Show logs for a CodeBuild build"
    echo ""
    echo "S3 Commands:"
    echo "  list-artifacts <env>            - List available releases in S3"
    echo ""
    echo "CodeDeploy Commands:"
    echo "  list-deployments <env>          - List recent CodeDeploy deployments"
    echo "  deployment-logs <env> <dep-id>  - Show detailed deployment events"
    echo ""
    echo "EC2 Commands:"
    echo "  instance-logs <env>             - Show EC2 UserData execution logs (via SSM)"
    echo ""
    echo "CloudFormation Commands:"
    echo "  stack-events <env> [max-items]  - Show CloudFormation stack events (default: 50)"
    echo "  list-stacks                     - List all CloudFormation stacks"
    echo ""
    echo "S3 Maintenance Commands:"
    echo "  empty-bucket <env>              - Manually empty bucket (for stuck DELETE_FAILED stacks)"
    echo ""
    echo "Application Commands:"
    echo "  ping <env> [message]            - Test application endpoint via ALB (default: 'ping')"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Examples:"
    echo "  $0 list-builds dev              # See recent builds"
    echo "  $0 logs dev abc123              # View build logs"
    echo "  $0 list-artifacts dev           # Find previous build-id for rollback"
    echo "  $0 list-deployments dev         # See recent deployments"
    echo "  $0 instance-logs dev            # Check UserData execution"
    echo "  $0 stack-events dev 100         # View stack events"
    echo "  $0 list-stacks                  # View all CloudFormation stacks"
    echo "  $0 empty-bucket dev             # Manually empty bucket (for DELETE_FAILED)"
    echo "  $0 ping dev                     # Test application endpoint"
    exit 1
}

# Helper functions
get_stack_name() {
    local env=$1
    echo "${STACK_PREFIX}-${env}"
}

get_stack_output() {
    local env=$1
    local output_key=$2
    local stack_name="${STACK_PREFIX}-${env}"

    aws cloudformation describe-stacks \
        --stack-name "$stack_name" \
        --query "Stacks[0].Outputs[?OutputKey=='$output_key'].OutputValue" \
        --output text 2>/dev/null
}

# Command implementations
cmd_list_builds() {
    local env=$1

    local codebuild_project=$(get_stack_output "$env" "CodeBuildProjectName")

    if [ -z "$codebuild_project" ]; then
        echo "Error: Could not get CodeBuild project name from stack outputs"
        exit 1
    fi

    echo "Recent CodeBuild builds for: $codebuild_project"
    echo ""

    # Get list of builds for this project
    local build_ids=$(aws codebuild list-builds-for-project \
        --project-name "$codebuild_project" \
        --sort-order DESCENDING \
        --max-items 20 \
        --query 'ids' \
        --output text)

    if [ -z "$build_ids" ]; then
        echo "No builds found."
        return
    fi

    # Get details for these builds
    aws codebuild batch-get-builds \
        --ids $build_ids \
        --query 'builds[*].[id,buildStatus,startTime,endTime]' \
        --output table
}

cmd_list_artifacts() {
    local env=$1

    local artifact_bucket=$(get_stack_output "$env" "ArtifactBucketName")

    if [ -z "$artifact_bucket" ]; then
        echo "Error: Could not get artifact bucket name from stack outputs"
        exit 1
    fi

    echo "Available releases in S3: s3://${artifact_bucket}/releases/"
    echo ""

    # List all release artifacts
    local artifacts=$(aws s3 ls "s3://${artifact_bucket}/releases/" --recursive | grep "\.tar\.gz$")

    if [ -z "$artifacts" ]; then
        echo "No release artifacts found."
        echo ""
        echo "Run: ./scripts/aws-deploy.sh build $env"
        return
    fi

    echo "$artifacts" | while read -r date time size path; do
        local build_id=$(echo "$path" | sed 's|releases/\([^/]*\)/.*|\1|')
        local filename=$(basename "$path")
        printf "%-20s  %-10s  %s  %s\n" "$date $time" "$size" "$build_id" "$filename"
    done
}

cmd_logs() {
    local env=$1
    local build_id=$2

    if [ -z "$build_id" ]; then
        echo "Error: build-id required"
        echo "Usage: $0 logs <env> <build-id>"
        echo ""
        echo "Get build-id from: $0 list-builds $env"
        exit 1
    fi

    local codebuild_project=$(get_stack_output "$env" "CodeBuildProjectName")

    if [ -z "$codebuild_project" ]; then
        echo "Error: Could not get CodeBuild project name from stack outputs"
        exit 1
    fi

    echo "Fetching logs for build: $build_id"
    echo "---"
    echo ""

    local log_group="/aws/codebuild/${codebuild_project}"
    local log_stream="${build_id#*:}"

    # Fetch all log events
    aws logs get-log-events \
        --log-group-name "$log_group" \
        --log-stream-name "$log_stream" \
        --start-from-head \
        --output json 2>/dev/null | jq -r '.events[]?.message' 2>/dev/null

    if [ $? -ne 0 ]; then
        echo "Error: Could not fetch logs for build $build_id"
        echo ""
        echo "Verify build-id with: $0 list-builds $env"
        exit 1
    fi
}

cmd_list_deployments() {
    local env=$1

    local codedeploy_app=$(get_stack_output "$env" "CodeDeployApplicationName")
    local deployment_group=$(get_stack_output "$env" "DeploymentGroupName")

    if [ -z "$codedeploy_app" ] || [ -z "$deployment_group" ]; then
        echo "Error: Could not get CodeDeploy details from stack outputs"
        exit 1
    fi

    echo "Recent CodeDeploy deployments for: $codedeploy_app / $deployment_group"
    echo ""

    # List recent deployments
    local deployment_ids=$(aws deploy list-deployments \
        --application-name "$codedeploy_app" \
        --deployment-group-name "$deployment_group" \
        --max-items 20 \
        --query 'deployments' \
        --output text)

    if [ -z "$deployment_ids" ]; then
        echo "No deployments found."
        return
    fi

    # Get deployment details
    aws deploy batch-get-deployments \
        --deployment-ids $deployment_ids \
        --query 'deploymentsInfo[*].[deploymentId,status,createTime,completeTime]' \
        --output table
}

cmd_deployment_logs() {
    local env=$1
    local deployment_id=$2

    if [ -z "$deployment_id" ]; then
        echo "Error: deployment-id required"
        echo "Usage: $0 deployment-logs <env> <deployment-id>"
        echo ""
        echo "Get deployment-id from: $0 list-deployments $env"
        exit 1
    fi

    echo "Fetching deployment details for: $deployment_id"
    echo "---"
    echo ""

    # Get full deployment details
    aws deploy get-deployment \
        --deployment-id "$deployment_id" \
        --output json | jq

    echo ""
    echo "Lifecycle events:"
    echo "---"
    echo ""

    # Get instance deployment details
    aws deploy list-deployment-instances \
        --deployment-id "$deployment_id" \
        --query 'instancesList' \
        --output text | while read instance_id; do
        echo "Instance: $instance_id"
        aws deploy get-deployment-instance \
            --deployment-id "$deployment_id" \
            --instance-id "$instance_id" \
            --query 'instanceSummary.lifecycleEvents' \
            --output json | jq
    done
}

cmd_instance_logs() {
    local env=$1

    local instance_id=$(get_stack_output "$env" "InstanceId")

    if [ -z "$instance_id" ]; then
        echo "Error: Could not get instance ID from stack outputs"
        exit 1
    fi

    echo "Fetching EC2 UserData execution logs for: $instance_id"
    echo "---"
    echo ""
    echo "Cloud-Init Output (UserData execution):"
    echo ""

    # Use SSM to fetch logs
    aws ssm send-command \
        --instance-ids "$instance_id" \
        --document-name "AWS-RunShellScript" \
        --parameters 'commands=["tail -100 /var/log/cloud-init-output.log"]' \
        --output text \
        --query 'Command.CommandId' > /tmp/ssm-command-id.txt

    local command_id=$(cat /tmp/ssm-command-id.txt)

    # Wait for command to complete
    sleep 3

    # Get command output
    aws ssm get-command-invocation \
        --command-id "$command_id" \
        --instance-id "$instance_id" \
        --query 'StandardOutputContent' \
        --output text

    echo ""
    echo "---"
    echo ""
    echo "Hello Erlang Init Log:"
    echo ""

    # Get our custom init log
    aws ssm send-command \
        --instance-ids "$instance_id" \
        --document-name "AWS-RunShellScript" \
        --parameters 'commands=["cat /var/log/hello-erlang-init.log 2>/dev/null || echo \"Init log not found\""]' \
        --output text \
        --query 'Command.CommandId' > /tmp/ssm-command-id2.txt

    local command_id2=$(cat /tmp/ssm-command-id2.txt)
    sleep 2

    aws ssm get-command-invocation \
        --command-id "$command_id2" \
        --instance-id "$instance_id" \
        --query 'StandardOutputContent' \
        --output text

    echo ""
    echo "---"
    echo ""
    echo "Agent Status:"
    echo ""

    # Check agent status
    aws ssm send-command \
        --instance-ids "$instance_id" \
        --document-name "AWS-RunShellScript" \
        --parameters 'commands=["systemctl status amazon-cloudwatch-agent --no-pager -l","echo ---","systemctl status codedeploy-agent --no-pager -l"]' \
        --output text \
        --query 'Command.CommandId' > /tmp/ssm-command-id3.txt

    local command_id3=$(cat /tmp/ssm-command-id3.txt)
    sleep 3

    aws ssm get-command-invocation \
        --command-id "$command_id3" \
        --instance-id "$instance_id" \
        --query 'StandardOutputContent' \
        --output text

    # Cleanup temp files
    rm -f /tmp/ssm-command-id*.txt
}

cmd_stack_events() {
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
    echo "To see more events: $0 stack-events $env 100"
}

cmd_list_stacks() {
    echo "CloudFormation Stacks:"
    echo ""

    aws cloudformation list-stacks \
        --query 'StackSummaries[?StackStatus!=`DELETE_COMPLETE`].[StackName,StackStatus,CreationTime]' \
        --output table
}

cmd_empty_bucket() {
    local env=$1
    local bucket_name="${env}-hello-erlang-artifacts-$(aws sts get-caller-identity --query Account --output text)"

    echo "Note: The 'delete' command now automatically empties buckets."
    echo "This manual command is only needed for stuck DELETE_FAILED stacks."
    echo ""
    echo "Emptying S3 bucket: $bucket_name"
    echo "This will delete all objects and versions from the bucket."
    echo ""
    read -p "Are you sure? (yes/no): " confirm

    if [ "$confirm" != "yes" ]; then
        echo "Operation cancelled"
        exit 0
    fi

    echo ""
    echo "Checking if bucket exists..."
    if ! aws s3api head-bucket --bucket "$bucket_name" 2>/dev/null; then
        echo "✓ Bucket does not exist (already deleted or never created)"
        exit 0
    fi

    echo "✓ Bucket exists"
    echo ""
    echo "Deleting all object versions..."

    # Delete all versions and delete markers
    aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --output json \
        --query '{Objects: Versions[].{Key:Key,VersionId:VersionId}}' | \
    jq -c '.Objects[]?' | \
    while read -r obj; do
        if [ -n "$obj" ]; then
            key=$(echo "$obj" | jq -r '.Key')
            version=$(echo "$obj" | jq -r '.VersionId')
            echo "  Deleting: $key (version: $version)"
            aws s3api delete-object \
                --bucket "$bucket_name" \
                --key "$key" \
                --version-id "$version" >/dev/null
        fi
    done

    # Delete all delete markers
    aws s3api list-object-versions \
        --bucket "$bucket_name" \
        --output json \
        --query '{Objects: DeleteMarkers[].{Key:Key,VersionId:VersionId}}' | \
    jq -c '.Objects[]?' | \
    while read -r obj; do
        if [ -n "$obj" ]; then
            key=$(echo "$obj" | jq -r '.Key')
            version=$(echo "$obj" | jq -r '.VersionId')
            echo "  Deleting marker: $key (version: $version)"
            aws s3api delete-object \
                --bucket "$bucket_name" \
                --key "$key" \
                --version-id "$version" >/dev/null
        fi
    done

    echo ""
    echo "✓ Bucket emptied successfully"
    echo ""
    echo "You can now retry stack deletion:"
    echo "  ./scripts/aws-stack.sh delete $env"
}

cmd_ping() {
    local env=$1
    local message="${2:-ping}"

    # Get ALB DNS name from stack outputs
    local alb_dns=$(get_stack_output "$env" "LoadBalancerDNS")
    if [ -z "$alb_dns" ]; then
        echo "Error: Could not get Load Balancer DNS for environment '$env'" >&2
        exit 1
    fi

    # URL encode the message
    local encoded_message=$(printf %s "$message" | jq -sRr @uri)

    local url="http://${alb_dns}/echo?message=${encoded_message}"

    echo "Testing application endpoint..."
    echo "  URL: $url"
    echo ""

    if ! command -v curl &> /dev/null; then
        echo "Error: curl not found. Please install curl."
        exit 1
    fi

    local response=$(curl -s -w "\n%{http_code}" --connect-timeout 5 --max-time 10 "$url" 2>/dev/null)
    local http_code=$(echo "$response" | tail -n 1)
    local body=$(echo "$response" | sed '$d')

    if [ "$http_code" == "200" ] && [ "$body" == "$message" ]; then
        echo "✓ Application is responding"
        echo "  Status: 200 OK"
        echo "  Response: $body"
    elif [ "$http_code" == "200" ]; then
        echo "⚠ Application responded but with unexpected content"
        echo "  Status: 200 OK"
        echo "  Response: $body"
        echo "  Expected: $message"
    elif [ -n "$http_code" ]; then
        echo "✗ Application returned error"
        echo "  Status: $http_code"
        echo "  Response: $body"
        exit 1
    else
        echo "✗ Application not responding"
        echo "  Could not connect to $url"
        exit 1
    fi
}

# Main command router
if [ -z "$1" ]; then
    usage
fi

COMMAND=$1

# Handle commands that don't require environment
if [ "$COMMAND" == "list-stacks" ]; then
    cmd_list_stacks
    exit 0
fi

# All other commands require environment
if [ -z "$2" ]; then
    usage
fi

ENV=$2
shift 2

case "$COMMAND" in
    list-builds)
        cmd_list_builds "$ENV"
        ;;
    list-artifacts)
        cmd_list_artifacts "$ENV"
        ;;
    logs)
        cmd_logs "$ENV" "$@"
        ;;
    list-deployments)
        cmd_list_deployments "$ENV"
        ;;
    deployment-logs)
        cmd_deployment_logs "$ENV" "$@"
        ;;
    instance-logs)
        cmd_instance_logs "$ENV"
        ;;
    stack-events)
        cmd_stack_events "$ENV" "$@"
        ;;
    empty-bucket)
        cmd_empty_bucket "$ENV"
        ;;
    ping)
        cmd_ping "$ENV" "$@"
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        echo ""
        usage
        ;;
esac
