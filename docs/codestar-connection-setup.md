# CodeStar Connection Setup Guide

## Overview

After deploying the CloudFormation stack, the CodeStar Connection for GitHub will be in **PENDING** state. You must complete a one-time OAuth handshake to activate it.

This guide walks you through activating the connection so CodePipeline can pull from your GitHub repository.

## Prerequisites

- Stack deployed successfully
- GitHub account with access to the repository
- AWS Console access

## Steps to Activate Connection

### 1. Get the Connection ARN

Check your stack outputs to find the connection ARN:

```bash
./scripts/aws-stack.sh outputs dev
```

Look for the `GitHubConnectionArn` output. You can also use the command shown in `GitHubConnectionStatus` output.

### 2. Navigate to AWS Console

Go to the Connections page in AWS Console:

- **Path**: AWS Console → Developer Tools → Settings → Connections
- **Direct link**: https://console.aws.amazon.com/codesuite/settings/connections

### 3. Find Your Connection

Look for your connection in the list:
- **Name**: `dev-hello-erlang-github` (or `staging`/`prod` depending on environment)
- **Status**: PENDING (shown in orange)
- **Provider**: GitHub

### 4. Update Pending Connection

Click on the connection name, then click the **"Update pending connection"** button.

### 5. Install GitHub App

You'll see options for GitHub app installation:

- **First time**: Click **"Install a new app"** - you'll be redirected to GitHub
- **Already installed**: Select the existing "AWS Connector for GitHub" installation

### 6. Authorize on GitHub

On the GitHub authorization page:

1. **Authorize the app**: Grant permissions to "AWS Connector for GitHub"

2. **Select repository access**:
   - Choose **"Only select repositories"** (recommended)
   - Select **`hello-erlang`** from the dropdown
   - Alternatively: "All repositories" (grants access to everything - not recommended)

3. **Save**: Click **"Save"** or **"Install"**

**For GitHub Organizations:**
- An organization admin must approve the AWS Connector app
- Go to: Organization → Settings → Third-party access
- Approve "AWS Connector for GitHub"

### 7. Complete Connection in AWS

After GitHub authorization, you'll be redirected back to AWS Console:

1. Click **"Connect"** to finalize the connection
2. The connection status will change from PENDING to **AVAILABLE** ✅

### 8. Verify Connection Status

Confirm the connection is active:

```bash
# Using AWS CLI
aws codestar-connections get-connection --connection-arn <your-connection-arn>

# Expected output:
# {
#     "Connection": {
#         "ConnectionName": "dev-hello-erlang-github",
#         "ConnectionStatus": "AVAILABLE",
#         "ProviderType": "GitHub",
#         ...
#     }
# }
```

Or check in the AWS Console - the status should show **Available** (green).

## Testing the Pipeline

Once the connection is AVAILABLE, test the pipeline:

```bash
# Push to the deploy branch for your environment
git push origin main:deploy/dev

# Watch the pipeline in AWS Console
# Or check pipeline state via CLI:
aws codepipeline get-pipeline-state --name dev-hello-erlang-pipeline
```

## Troubleshooting

### Connection stays PENDING

**Problem**: After completing OAuth, connection remains PENDING

**Solution**:
- Ensure you clicked "Connect" in the AWS Console after GitHub redirect
- Check browser console for errors
- Try again: "Update pending connection" → complete flow again

### "Repository not found" error in Source stage

**Problem**: Pipeline source stage fails with repository access error

**Possible causes:**

1. **GitHub app not granted access to repository**
   - Go to GitHub → Settings → Applications → AWS Connector for GitHub
   - Edit repository access → Add `hello-erlang`

2. **Wrong repository name format**
   - Must be: `owner/repo` (e.g., `my-org/hello-erlang`)
   - NOT: `https://github.com/my-org/hello-erlang`

3. **Private repository without access**
   - Ensure AWS Connector app has access to private repos
   - Check GitHub app installation settings

### Organization permission denied

**Problem**: "This organization has not approved the AWS Connector app"

**Solution**:
1. GitHub org admin must approve the third-party app
2. Go to: GitHub → Organization → Settings → Third-party access
3. Find "AWS Connector for GitHub" → Grant access
4. Retry connection setup in AWS

### Connection works but pipeline still fails

**Problem**: Connection status is AVAILABLE but source stage fails

**Check these:**

1. **Verify branch exists**:
   ```bash
   git ls-remote --heads origin deploy/dev
   ```

2. **Verify CloudFormation parameters**:
   ```bash
   aws cloudformation describe-stacks \
       --stack-name hello-erlang-dev \
       --query 'Stacks[0].Parameters[?ParameterKey==`GitHubBranch`]'
   ```

3. **Check pipeline IAM role has UseConnection permission**:
   ```bash
   aws iam get-role-policy \
       --role-name dev-hello-erlang-pipeline-role \
       --policy-name PipelinePolicy
   ```

## Connection Reuse

**Good news**: CodeStar Connections can be shared across pipelines!

- ✅ One connection per AWS account/region can serve multiple pipelines
- ✅ No need to repeat OAuth for each environment (dev/staging/prod)
- ✅ To reuse: Reference the same connection ARN in other stacks

However, for isolation and clarity, our setup creates one connection per environment.

## Security Notes

- 🔒 OAuth-based authentication (no static tokens)
- 🔒 Scoped to specific repositories
- 🔒 Never expires (AWS manages credentials automatically)
- 🔒 Revocable from either AWS Console or GitHub settings
- 🔒 Audit trail visible in both AWS CloudTrail and GitHub admin console

## Additional Resources

- [AWS CodeStar Connections Documentation](https://docs.aws.amazon.com/codepipeline/latest/userguide/connections.html)
- [GitHub Apps Documentation](https://docs.github.com/en/developers/apps/getting-started-with-apps/about-apps)
- [Troubleshooting CodePipeline Source Actions](https://docs.aws.amazon.com/codepipeline/latest/userguide/troubleshooting.html#troubleshooting-gs2)
