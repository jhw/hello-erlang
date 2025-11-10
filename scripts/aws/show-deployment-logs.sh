#!/bin/bash
# Show CodeDeploy deployment details and lifecycle events

set -e

cd "$(dirname "$0")/../.."

usage() {
    echo "Usage: $0 <environment> <deployment-id>"
    echo ""
    echo "Show CodeDeploy deployment details and lifecycle events."
    echo ""
    echo "Environments: dev, staging, prod"
    echo "Deployment ID: Get from list-deployments.sh output"
    echo ""
    echo "Example:"
    echo "  $0 dev d-ABC123DEF"
    exit 1
}

if [ $# -lt 2 ]; then
    usage
fi

ENV=$1
DEPLOYMENT_ID=$2

case "$ENV" in
    dev|staging|prod)
        ;;
    *)
        echo "Error: Invalid environment: $ENV"
        exit 1
        ;;
esac

echo "Fetching deployment details for: $DEPLOYMENT_ID"
echo "---"
echo ""

# Get full deployment details
aws deploy get-deployment \
    --deployment-id "$DEPLOYMENT_ID" \
    --output json | jq

echo ""
echo "Lifecycle events:"
echo "---"
echo ""

# Get instance deployment details
aws deploy list-deployment-instances \
    --deployment-id "$DEPLOYMENT_ID" \
    --query 'instancesList' \
    --output text | while read INSTANCE_ID; do
    echo "Instance: $INSTANCE_ID"
    aws deploy get-deployment-instance \
        --deployment-id "$DEPLOYMENT_ID" \
        --instance-id "$INSTANCE_ID" \
        --query 'instanceSummary.lifecycleEvents' \
        --output json | jq
done
