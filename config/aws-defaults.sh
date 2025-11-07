#!/bin/bash
# AWS configuration defaults for hello-erlang project
#
# This file provides project-specific defaults that override ~/.aws/config
# Create config/aws-local.sh to override these settings locally (gitignored)

# AWS Region - where to deploy resources
# Override with: export AWS_REGION=us-west-2
export AWS_REGION="${AWS_REGION:-us-east-1}"

# AWS Profile - which credentials to use
# Override with: export AWS_PROFILE=myprofile
export AWS_PROFILE="${AWS_PROFILE:-default}"

# Stack name prefix - used for CloudFormation stacks
# Results in: hello-erlang-dev, hello-erlang-staging, hello-erlang-prod
export STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

# Default instance type for new stacks
export DEFAULT_INSTANCE_TYPE="${DEFAULT_INSTANCE_TYPE:-t3.small}"

# Default SSH access (use your IP for security: x.x.x.x/32)
export DEFAULT_SSH_LOCATION="${DEFAULT_SSH_LOCATION:-0.0.0.0/0}"

# Deployment directory on EC2
export DEPLOY_DIR="${DEPLOY_DIR:-/opt/hello_erlang}"

# Application port (Cowboy)
export APP_PORT="${APP_PORT:-8080}"

# Echo configuration values (helpful for debugging)
if [ "${AWS_CONFIG_DEBUG:-false}" = "true" ]; then
    echo "AWS Configuration:"
    echo "  Region: $AWS_REGION"
    echo "  Profile: $AWS_PROFILE"
    echo "  Stack Prefix: $STACK_PREFIX"
    echo "  Instance Type: $DEFAULT_INSTANCE_TYPE"
    echo "  SSH Location: $DEFAULT_SSH_LOCATION"
fi
