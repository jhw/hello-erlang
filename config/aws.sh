#!/bin/bash
# AWS Configuration
#
# Optional configuration for AWS deployment.
# Most settings auto-discover - only set what you need to override.

# AWS CLI Configuration
export AWS_REGION=eu-west-1
export AWS_PROFILE=woldeploy

# Optional Overrides (uncomment to customize)
# export DEFAULT_KEY_NAME=jhw-keypair-x240  # Force specific SSH key
# export DEFAULT_INSTANCE_TYPE=t3.small
# export DEFAULT_SSH_LOCATION=0.0.0.0/0
# export DEFAULT_ALB_SUBNETS=subnet-xxxxx,subnet-yyyyy
# export STACK_PREFIX=my-erlang-app
# export DEPLOY_DIR=/opt/my_app  
