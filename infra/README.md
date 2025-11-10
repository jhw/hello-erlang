# Infrastructure Directory

This directory contains all AWS infrastructure configuration and Lambda function code for the hello-erlang deployment.

## Directory Structure

```
infra/
├── README.md                    # This file
├── stack.yaml                   # Main CloudFormation template
├── buildspec.yml                # CodeBuild specification
├── appspec.yml                  # CodeDeploy specification
├── codedeploy/                  # CodeDeploy lifecycle hooks
│   ├── before_install.sh
│   ├── after_install.sh
│   ├── start_application.sh
│   ├── stop_application.sh
│   └── validate_service.sh
└── handlers/                    # Lambda function code
    ├── app_error_notifier.py    # Application error notification Lambda
    └── pipeline_notifier.py     # Pipeline event notification Lambda
```

## CloudFormation Template

**File:** `stack.yaml`
**Size:** ~957 lines, ~31KB
**Deployment:** Uploaded to S3 (no size limit)

The main CloudFormation template defines the complete deployment infrastructure.

**Note:** The template is automatically uploaded to the stack-artifacts S3 bucket during deployment, removing the 51KB CloudFormation size limit and enabling template versioning.

### Resources Created

- **Compute**: EC2 instance, Application Load Balancer
- **CI/CD**: CodePipeline, CodeBuild, CodeDeploy
- **Storage**: S3 buckets (pipeline-artifacts, stack-artifacts)
- **Monitoring**: Lambda functions, CloudWatch Logs, EventBridge
- **Networking**: Security Groups, Target Groups
- **IAM**: Roles and policies for all services

### Deployment

The CloudFormation template is automatically uploaded to S3 during deployment. This provides:
- **No size limit** (not constrained by CloudFormation's 51KB limit)
- **Automatic versioning** via S3
- **Template history** for rollback and auditing

Deploy the stack using the management script:

```bash
# Prerequisites (first time only)
./scripts/aws/artifacts.sh create dev
./scripts/aws/deploy-handlers.sh dev

# Deploy new stack (template automatically uploaded to S3)
./scripts/aws/stack.sh deploy dev

# Update existing stack (new template version uploaded to S3)
./scripts/aws/stack.sh update dev
```

## Lambda Handlers

Lambda functions are now stored as **external Python files** and deployed to S3, not embedded inline in the CloudFormation template.

### Available Handlers

| Handler | Purpose | Trigger |
|---------|---------|---------|
| `app_error_notifier.py` | Monitors application errors and sends Slack notifications | CloudWatch Logs subscription |
| `pipeline_notifier.py` | Monitors pipeline events and sends Slack notifications | EventBridge rule |

### Deploying Lambda Handlers

Before deploying or updating the CloudFormation stack, you must upload the Lambda handlers to S3:

```bash
# 1. Create the stack-artifacts bucket (first time only)
./scripts/aws/artifacts.sh create dev

# 2. Package and upload Lambda handlers
./scripts/aws/deploy-handlers.sh dev

# 3. Deploy or update the stack
./scripts/aws/stack.sh deploy dev
```

The deployment script:
1. Packages each Python handler into a ZIP file
2. Uploads to the stack-artifacts S3 bucket
3. Bucket versioning automatically tracks deployments

### Handler Development

To modify a Lambda function:

1. Edit the Python file in `infra/handlers/`
2. Deploy handlers: `./scripts/aws/deploy-handlers.sh <env>`
3. Update the CloudFormation stack: `./scripts/aws/stack.sh update <env>`

Lambda functions will automatically use the latest uploaded code.

## S3 Buckets

The infrastructure uses two separate S3 buckets:

### Pipeline Artifacts Bucket

- **Name Pattern**: `{env}-hello-erlang-pipeline-artifacts-{AccountId}`
- **Purpose**: Stores CodeBuild outputs and CodePipeline artifacts
- **Versioning**: Enabled
- **Lifecycle**: 30-day retention
- **Managed by**: CloudFormation stack

### Stack Artifacts Bucket

- **Name Pattern**: `{env}-hello-erlang-stack-artifacts-{AccountId}`
- **Purpose**: Stores Lambda deployment packages and CloudFormation templates
- **Contents**:
  - `stack.yaml` - CloudFormation template (versioned)
  - `app_error_notifier.zip` - Lambda handler (versioned)
  - `pipeline_notifier.zip` - Lambda handler (versioned)
- **Versioning**: Enabled
- **Lifecycle**: 90-day retention for old versions
- **Managed by**: `scripts/aws/artifacts.sh` script

```bash
# Bucket management
./scripts/aws/artifacts.sh create dev      # Create bucket
./scripts/aws/artifacts.sh list dev        # List contents
./scripts/aws/artifacts.sh empty dev       # Empty bucket
./scripts/aws/artifacts.sh delete dev      # Delete bucket
```

## CodeBuild Specification

**File:** `buildspec.yml`

Defines the build process for Erlang releases:

1. **Install**: Activate pre-installed Erlang/OTP
2. **Build**: Compile release with `rebar3 as prod tar`
3. **Post-build**: Extract release, add CodeDeploy files, upload to S3

Build artifacts are stored in the **pipeline-artifacts** bucket.

## CodeDeploy Specification

**File:** `appspec.yml`

Defines how the Erlang application is deployed to EC2:

- **Destination**: `/opt/hello_erlang`
- **Lifecycle Hooks**: See `codedeploy/` directory

### Lifecycle Hooks

| Hook | Script | Purpose |
|------|--------|---------|
| ApplicationStop | `stop_application.sh` | Stop running Erlang application |
| BeforeInstall | `before_install.sh` | Backup current release, prepare directories |
| AfterInstall | `after_install.sh` | Validate release structure, set permissions |
| ApplicationStart | `start_application.sh` | Start Erlang application in daemon mode |
| ValidateService | `validate_service.sh` | Health checks with retries |

## Migration from Previous Structure

This directory replaces the old `config/aws/` structure:

| Old Path | New Path |
|----------|----------|
| `config/aws/stack.yaml` | `infra/stack.yaml` |
| `config/aws/buildspec.yml` | `infra/buildspec.yml` |
| `config/aws/appspec.yml` | `infra/appspec.yml` |
| `config/aws/codedeploy/` | `infra/codedeploy/` |
| *(inline in stack.yaml)* | `infra/handlers/` |

### Benefits of New Structure

1. **Reduced template size**: 28% smaller (31KB vs 43KB)
2. **Scalability**: Lambda functions no longer limited by template size
3. **Modularity**: Handlers can be developed and deployed independently
4. **Clarity**: Clearer separation between infrastructure and code
5. **Future-proof**: Room for growth without hitting CloudFormation limits

## Environment Configuration

Environment-specific configuration is managed via CloudFormation parameters:

- **Environment**: dev, staging, prod
- **InstanceType**: EC2 instance size
- **ErlangVersion**: Erlang/OTP version for builds
- **GitHub**: Repository owner, name, branch
- **Slack**: Webhook URLs for notifications

Configure these in `config/env.sh` (gitignored) or pass as parameters.

## Monitoring and Notifications

### Application Errors

CloudWatch Logs subscription filter monitors `/aws/ec2/hello-erlang/{env}` for:
- ERROR, error, Exception, crash, failed, timeout, supervisor_report

Matching events trigger the `app_error_notifier` Lambda → Slack notification.

### Pipeline Events

EventBridge rule monitors CodePipeline state changes:
- Pipeline execution: STARTED, SUCCEEDED, FAILED
- Stage execution: STARTED, SUCCEEDED, FAILED

Events trigger the `pipeline_notifier` Lambda → Slack notification.

## Related Documentation

- [`../docs/codestar-connection-setup.md`](../docs/codestar-connection-setup.md) - GitHub connection setup
- [`../docs/logging-strategy.md`](../docs/logging-strategy.md) - Logging architecture
- [`../scripts/aws/README.md`](../scripts/aws/README.md) - Deployment scripts

## Troubleshooting

### CloudFormation Stack Update Fails

**Issue**: Stack update fails with "S3 object does not exist"

**Solution**: Deploy Lambda handlers first:
```bash
./scripts/aws/deploy-handlers.sh dev
./scripts/aws/stack.sh update dev
```

### Lambda Function Not Updating

**Issue**: Lambda function code doesn't reflect recent changes

**Solution**: S3 versioning may be caching old versions:
```bash
# Re-deploy handlers
./scripts/aws/deploy-handlers.sh dev

# Force Lambda to refresh by updating stack
./scripts/aws/stack.sh update dev
```

### Stack-Artifacts Bucket Doesn't Exist

**Issue**: `deploy-handlers.sh` fails with bucket not found

**Solution**: Create the bucket first:
```bash
./scripts/aws/artifacts.sh create dev
```
