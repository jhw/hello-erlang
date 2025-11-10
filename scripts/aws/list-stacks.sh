#!/bin/bash
# List all CloudFormation stacks

set -e

echo "All CloudFormation stacks:"
echo ""

aws cloudformation list-stacks \
    --stack-status-filter CREATE_COMPLETE UPDATE_COMPLETE ROLLBACK_COMPLETE UPDATE_ROLLBACK_COMPLETE \
    --query 'StackSummaries[*].[StackName,StackStatus,CreationTime]' \
    --output table
