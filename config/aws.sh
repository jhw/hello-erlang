#!/bin/bash
# AWS Configuration
#
# This file provides default configuration for AWS deployment scripts.
# Edit the values below to customize for your environment.
# All settings are optional - uncomment and modify as needed.

# AWS CLI Configuration
# Uncomment to use a different AWS region
# export AWS_REGION=us-west-2

# Uncomment to use a specific AWS profile
# export AWS_PROFILE=work

# Stack Configuration
# Uncomment to override stack name prefix
# export STACK_PREFIX=my-erlang-app

# Uncomment to use a different deployment directory on EC2
# export DEPLOY_DIR=/opt/my_app

# CloudFormation Parameter Defaults
# These can be overridden by command-line arguments

# Default instance type (t3.micro, t3.small, t3.medium, t3.large)
# export DEFAULT_INSTANCE_TYPE=t3.small

# Default SSH access CIDR (use your IP for better security, e.g., 1.2.3.4/32)
# export DEFAULT_SSH_LOCATION=0.0.0.0/0

# Default key pair name (if you always use the same key)
# export DEFAULT_KEY_NAME=my-default-key

# Default ALB subnets (comma-separated, at least 2 in different AZs)
# Get your subnet IDs with:
#   aws ec2 describe-subnets --filters "Name=default-for-az,Values=true" \
#     --query 'Subnets[*].[SubnetId,AvailabilityZone]' --output table
# export DEFAULT_ALB_SUBNETS=subnet-xxxxx,subnet-yyyyy
