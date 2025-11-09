# CodePipeline Migration Plan

## Overview

This document outlines the migration from script-based deployment (`aws-deploy.sh`) to AWS CodePipeline with GitOps workflow. The migration enables automated deployments triggered by git push operations while maintaining our existing build and deployment processes.

## Current State vs Target State

### Current Architecture
```
Developer
    ↓ (runs script)
aws-deploy.sh
    ↓ (creates zip, uploads to S3)
CodeBuild (triggered manually)
    ↓ (builds release, bundles appspec + scripts)
S3 Artifact
    ↓ (EventBridge + Lambda trigger)
CodeDeploy (automated)
    ↓
EC2 Instance
```

### Target Architecture (GitOps)
```
Developer
    ↓ (git push origin deploy/dev)
GitHub Repository
    ↓ (webhook trigger)
CodePipeline
    ├─ Source Stage (GitHub)
    ├─ Build Stage (CodeBuild - reuses existing project)
    └─ Deploy Stage (CodeDeploy - reuses existing config)
        ↓
EC2 Instance

All stages → CloudWatch Logs
```

## GitOps Branch Strategy

### Branch-to-Environment Mapping

```
deploy/dev      → dev environment pipeline
deploy/staging  → staging environment pipeline
deploy/prod     → prod environment pipeline
```

### Workflow Example
```bash
# Developer workflow
git checkout main
# ... make changes ...
git commit -m "Add new feature"

# Deploy to dev
git push origin main:deploy/dev

# After testing, promote to staging
git push origin deploy/dev:deploy/staging

# After validation, promote to prod
git push origin deploy/staging:deploy/prod
```

### Why This Approach?
- ✅ **Explicit deployments**: Clear intent (push to `deploy/*` branch)
- ✅ **Environment isolation**: Each environment has dedicated branch
- ✅ **GitOps best practice**: Infrastructure state in Git
- ✅ **Rollback capability**: Revert by pushing previous commit
- ✅ **Audit trail**: Git history shows who deployed what, when
- ✅ **No main branch pollution**: `main` stays clean, `deploy/*` are deployment triggers

## GitHub Authentication Strategy

### Recommended: AWS CodeStar Connections (OAuth-based)

**This is the modern, secure best practice for connecting CodePipeline to GitHub.**

#### Why CodeStar Connections?

- ✅ **OAuth-based**: No static tokens to manage or rotate
- ✅ **Never expires**: AWS manages credentials automatically
- ✅ **Secure**: Scoped to specific repositories, not your entire GitHub account
- ✅ **Native GitHub Apps**: Uses GitHub's recommended integration method
- ✅ **Works with private repos**: Full support without security compromises
- ✅ **CloudFormation native**: First-class AWS support
- ✅ **Audit trail**: Visible in both AWS and GitHub admin consoles
- ✅ **No maintenance**: Set it once, forget it

#### How CodeStar Connections Work

```
1. Deploy CloudFormation stack
   └─> Connection created in "PENDING" state

2. Go to AWS Console → Developer Tools → Connections
   └─> Click "Update pending connection"

3. Complete GitHub OAuth flow (30 seconds)
   └─> Authorize "AWS Connector for GitHub" app
   └─> Select which repositories to grant access to

4. Connection state changes to "AVAILABLE"
   └─> CodePipeline can now access those repositories
```

### Alternative: Personal Access Tokens (NOT RECOMMENDED)

**⚠️  Deprecated approach - only use for temporary testing**

Personal Access Tokens (PATs) have significant drawbacks:
- ❌ Expire after 90 days to 1 year (requires manual rotation)
- ❌ Broad permissions (access to your entire GitHub account)
- ❌ Static secrets that can leak
- ❌ Deprecated by both AWS and GitHub
- ❌ More complex setup (Secrets Manager + CloudFormation)

If you must use PATs (e.g., for GitHub Enterprise Server on-premises), see the "Legacy Authentication Methods" section at the end of this document.

## Configuration in env.sh

### New Parameters
Add to `config/env.sh` (gitignored):

```bash
# GitHub Repository Configuration (for CodePipeline)
GITHUB_OWNER="your-org-or-username"
GITHUB_REPO="hello-erlang"

# Branch configuration per environment
DEV_BRANCH="deploy/dev"
STAGING_BRANCH="deploy/staging"
PROD_BRANCH="deploy/prod"
```

**Note**: No GitHub token required when using CodeStar Connections!

## buildspec.yml, appspec.yml, and CodeDeploy Scripts

### The Key Insight: Bundling Strategy Stays the Same

Your concern about CodeDeploy scripts being "part of the stack, not the app" is valid, but the **current bundling approach already solves this**:

```
Application Repository (hello-erlang)
├── apps/                          # Application code
├── config/
│   └── aws/
│       ├── buildspec.yml          # Build instructions (moves to root)
│       ├── appspec.yml            # Deployment spec (bundled during build)
│       └── codedeploy/            # Deployment scripts (bundled during build)
│           ├── start_application.sh
│           ├── stop_application.sh
│           ├── before_install.sh
│           ├── after_install.sh
│           └── validate_service.sh
├── rebar.config                   # App dependencies
└── buildspec.yml                  # → MOVE HERE (CodePipeline requirement)
```

### Why This Works

**CodeDeploy scripts ARE infrastructure code**, and they live in `config/aws/`. They're **not** mixed with application code in `apps/`. The build process (`buildspec.yml`) bundles them:

```yaml
# buildspec.yml (lines 59-72)
post_build:
  commands:
    # Extract Erlang release
    - tar -xzf "$TARBALL" -C bundle/

    # Bundle deployment infrastructure (appspec + scripts)
    - cp config/aws/appspec.yml bundle/appspec.yml
    - mkdir -p bundle/scripts
    - cp config/aws/codedeploy/*.sh bundle/scripts/

    # Create final artifact
    - cd bundle && tar -czf ../hello_erlang.tar.gz . && cd ..
```

### What Changes with CodePipeline

**Only one change needed**: Move `buildspec.yml` to repository root.

```bash
# Migration step
mv config/aws/buildspec.yml buildspec.yml

# Update paths in buildspec.yml
# Line 64: cp config/aws/appspec.yml → stays the same (still in config/aws/)
# Line 66: cp config/aws/codedeploy/*.sh → stays the same (still in config/aws/)
```

**Everything else stays identical**:
- ✅ `appspec.yml` stays in `config/aws/`
- ✅ CodeDeploy scripts stay in `config/aws/codedeploy/`
- ✅ buildspec.yml continues to bundle them during build
- ✅ Clear separation: app code in `apps/`, infrastructure in `config/aws/`

### Why NOT Put Scripts in CloudFormation

You might think: "Can CloudFormation deploy the scripts to S3, and CodeDeploy fetch them?"

**This doesn't work** because:
1. ❌ CodeDeploy expects `appspec.yml` + scripts **inside the deployment artifact**
2. ❌ No mechanism to inject external scripts during deployment
3. ❌ The artifact structure is mandated by CodeDeploy:
   ```
   hello_erlang.tar.gz/
   ├── appspec.yml           # MUST be at root of artifact
   ├── scripts/              # MUST be relative to appspec.yml
   │   ├── start_application.sh
   │   └── ...
   └── bin/, lib/, erts-*/   # Your application
   ```

### Alternative: Separate Infrastructure Repository (NOT RECOMMENDED)

You *could* split into two repos:

```
Repo 1: hello-erlang (application only)
Repo 2: hello-erlang-infrastructure (CloudFormation, appspec, scripts)
```

**Why this is worse**:
- ❌ Version synchronization nightmare (which app version needs which deploy scripts?)
- ❌ More complex CI/CD (two repos to coordinate)
- ❌ Harder to test (can't test app + deployment together)
- ❌ Against monorepo best practices for smaller projects

**Current approach is better**:
- ✅ Single source of truth
- ✅ App and deployment scripts version together
- ✅ Easy to test locally (everything in one checkout)
- ✅ Logical separation via directory structure (`apps/` vs `config/aws/`)

## What Happens to aws-deploy.sh

### Functions That Disappear

These are **replaced by CodePipeline**:

```bash
# Lines 82-95: Create source bundle
# ❌ NO LONGER NEEDED - CodePipeline pulls from GitHub directly

# Lines 100-104: Upload source to S3
# ❌ NO LONGER NEEDED - CodePipeline manages source artifacts

# Lines 107-114: Start CodeBuild
# ❌ NO LONGER NEEDED - CodePipeline triggers build automatically

# Lines 125-183: Tail build logs
# ❌ NO LONGER NEEDED - View in CodePipeline console or CloudWatch
```

### What Replaces It

**Deployment becomes**:
```bash
# Old way
./scripts/aws-deploy.sh build dev

# New way
git push origin main:deploy/dev
```

**Monitoring becomes**:
```bash
# Old way (manual log tailing in script)
./scripts/aws-deploy.sh build dev  # watches logs inline

# New way (CloudWatch + Pipeline console)
aws codepipeline get-pipeline-state --name hello-erlang-dev
aws logs tail /aws/codepipeline/hello-erlang-dev --follow
```

### Functions That Stay (in aws-debug.sh)

Debugging commands remain useful:
- ✅ `ping` - Test application endpoint
- ✅ `list-builds` - View build history
- ✅ `list-deployments` - View deployment history
- ✅ `deployment-logs` - Debug failed deployments
- ✅ `instance-logs` - SSH to instance (troubleshooting)

These work identically whether triggered by script or CodePipeline.

## CloudWatch Logging Strategy

### Current Logging (Script-Based)

```
/aws/codebuild/${Environment}-hello-erlang-build
    └── <build-id> (streams)

/aws/codedeploy/${Environment}/agent
/aws/codedeploy/${Environment}/deployments
/aws/codedeploy/${Environment}/updates
```

### Enhanced Logging (CodePipeline)

Add dedicated log groups for pipeline orchestration:

```
# Pipeline execution logs
/aws/codepipeline/${Environment}-hello-erlang-pipeline
    └── Source, Build, Deploy stage events

# Stage-specific logs (existing)
/aws/codebuild/${Environment}-hello-erlang-build
    └── Build output (already exists)

/aws/codedeploy/${Environment}/agent
/aws/codedeploy/${Environment}/deployments
/aws/codedeploy/${Environment}/updates
    └── Deployment lifecycle hooks (already exists)

# New: Pipeline state changes
/aws/events/codepipeline/${Environment}-hello-erlang
    └── Pipeline started, stage completed, failed, etc.
```

### Implementation in CloudFormation

```yaml
# CloudWatch Log Groups for CodePipeline
PipelineLogGroup:
  Type: AWS::Logs::LogGroup
  Properties:
    LogGroupName: !Sub '/aws/codepipeline/${Environment}-hello-erlang-pipeline'
    RetentionInDays: 14

PipelineEventsLogGroup:
  Type: AWS::Logs::LogGroup
  Properties:
    LogGroupName: !Sub '/aws/events/codepipeline/${Environment}-hello-erlang'
    RetentionInDays: 7

# EventBridge Rule to capture pipeline events
PipelineEventsRule:
  Type: AWS::Events::Rule
  Properties:
    Name: !Sub '${Environment}-hello-erlang-pipeline-events'
    Description: Log all CodePipeline state changes
    State: ENABLED
    EventPattern:
      source:
        - aws.codepipeline
      detail:
        pipeline:
          - !Ref CodePipeline
    Targets:
      - Arn: !GetAtt PipelineEventsLogGroup.Arn
        Id: CloudWatchLogsTarget
```

### Unified Logging View

**All logs accessible via**:
```bash
# Watch entire pipeline execution
aws logs tail /aws/codepipeline/dev-hello-erlang-pipeline --follow

# Watch specific stages
aws logs tail /aws/codebuild/dev-hello-erlang-build --follow
aws logs tail /aws/codedeploy/dev/deployments --follow

# Search across all logs
aws logs filter-log-events \
    --log-group-name /aws/codepipeline/dev-hello-erlang-pipeline \
    --filter-pattern "ERROR"
```

## CloudFormation Stack Changes

### New Resources to Add

```yaml
# 1. CodePipeline Service Role
CodePipelineServiceRole:
  Type: AWS::IAM::Role
  Properties:
    RoleName: !Sub '${Environment}-hello-erlang-pipeline-role'
    AssumeRolePolicyDocument:
      Version: '2012-10-17'
      Statement:
        - Effect: Allow
          Principal:
            Service: codepipeline.amazonaws.com
          Action: sts:AssumeRole
    Policies:
      - PolicyName: PipelinePolicy
        PolicyDocument:
          Version: '2012-10-17'
          Statement:
            # S3 artifacts
            - Effect: Allow
              Action:
                - s3:GetObject
                - s3:PutObject
                - s3:GetObjectVersion
              Resource: !Sub '${ArtifactBucket.Arn}/*'
            # CodeBuild
            - Effect: Allow
              Action:
                - codebuild:BatchGetBuilds
                - codebuild:StartBuild
              Resource: !GetAtt CodeBuildProject.Arn
            # CodeDeploy
            - Effect: Allow
              Action:
                - codedeploy:CreateDeployment
                - codedeploy:GetDeployment
                - codedeploy:GetDeploymentConfig
                - codedeploy:GetApplicationRevision
                - codedeploy:RegisterApplicationRevision
              Resource:
                - !Sub 'arn:aws:codedeploy:${AWS::Region}:${AWS::AccountId}:application/${CodeDeployApplication}'
                - !Sub 'arn:aws:codedeploy:${AWS::Region}:${AWS::AccountId}:deploymentgroup:${CodeDeployApplication}/${CodeDeployDeploymentGroup}'
            # CodeStar Connections (for GitHub access)
            - Effect: Allow
              Action:
                - codestar-connections:UseConnection
              Resource: !Ref GitHubConnection

# 2. GitHub Connection (OAuth-based, recommended)
GitHubConnection:
  Type: AWS::CodeStarConnections::Connection
  Properties:
    ConnectionName: !Sub '${Environment}-hello-erlang-github'
    ProviderType: GitHub
    # Note: Connection will be created in PENDING state
    # You must complete OAuth handshake in AWS Console (see Migration Steps)
    Tags:
      - Key: Environment
        Value: !Ref Environment
      - Key: Application
        Value: hello-erlang

# 3. CodePipeline
CodePipeline:
  Type: AWS::CodePipeline::Pipeline
  Properties:
    Name: !Sub '${Environment}-hello-erlang-pipeline'
    RoleArn: !GetAtt CodePipelineServiceRole.Arn
    ArtifactStore:
      Type: S3
      Location: !Ref ArtifactBucket
    Stages:
      # Source Stage
      - Name: Source
        Actions:
          - Name: SourceAction
            ActionTypeId:
              Category: Source
              Owner: AWS
              Provider: CodeStarSourceConnection
              Version: 1
            Configuration:
              ConnectionArn: !Ref GitHubConnection
              FullRepositoryId: !Sub '${GitHubOwner}/${GitHubRepo}'
              BranchName: !Ref GitHubBranch
              OutputArtifactFormat: CODE_ZIP
            OutputArtifacts:
              - Name: SourceOutput

      # Build Stage (reuses existing CodeBuild project)
      - Name: Build
        Actions:
          - Name: BuildAction
            ActionTypeId:
              Category: Build
              Owner: AWS
              Provider: CodeBuild
              Version: 1
            Configuration:
              ProjectName: !Ref CodeBuildProject
            InputArtifacts:
              - Name: SourceOutput
            OutputArtifacts:
              - Name: BuildOutput

      # Deploy Stage (reuses existing CodeDeploy config)
      - Name: Deploy
        Actions:
          - Name: DeployAction
            ActionTypeId:
              Category: Deploy
              Owner: AWS
              Provider: CodeDeploy
              Version: 1
            Configuration:
              ApplicationName: !Ref CodeDeployApplication
              DeploymentGroupName: !Ref CodeDeployDeploymentGroup
            InputArtifacts:
              - Name: BuildOutput

# 4. CloudWatch Event Rule (pipeline state changes)
PipelineEventRule:
  Type: AWS::Events::Rule
  Properties:
    Name: !Sub '${Environment}-hello-erlang-pipeline-events'
    Description: Capture CodePipeline execution events
    State: ENABLED
    EventPattern:
      source:
        - aws.codepipeline
      detail-type:
        - CodePipeline Pipeline Execution State Change
        - CodePipeline Stage Execution State Change
        - CodePipeline Action Execution State Change
      detail:
        pipeline:
          - !Ref CodePipeline
    Targets:
      - Arn: !Sub 'arn:aws:logs:${AWS::Region}:${AWS::AccountId}:log-group:/aws/events/codepipeline/${Environment}-hello-erlang'
        Id: CloudWatchLogTarget
```

### Parameters to Add

```yaml
Parameters:
  # ... existing parameters ...

  GitHubOwner:
    Type: String
    Description: GitHub repository owner (organization or username)

  GitHubRepo:
    Type: String
    Default: hello-erlang
    Description: GitHub repository name

  GitHubBranch:
    Type: String
    Description: Branch to monitor (e.g., deploy/dev, deploy/staging, deploy/prod)
```

### Outputs to Add

```yaml
Outputs:
  # ... existing outputs ...

  GitHubConnectionArn:
    Description: CodeStar Connection ARN for GitHub (complete OAuth in console)
    Value: !Ref GitHubConnection
    Export:
      Name: !Sub '${AWS::StackName}-GitHubConnectionArn'

  GitHubConnectionStatus:
    Description: Check connection status - must be AVAILABLE for pipeline to work
    Value: !Sub 'aws codestar-connections get-connection --connection-arn ${GitHubConnection}'

  PipelineName:
    Description: CodePipeline name
    Value: !Ref CodePipeline
    Export:
      Name: !Sub '${AWS::StackName}-PipelineName'

  PipelineUrl:
    Description: Direct link to pipeline in AWS Console
    Value: !Sub 'https://console.aws.amazon.com/codesuite/codepipeline/pipelines/${CodePipeline}/view'
```

### Resources to Remove

```yaml
# ❌ Remove Lambda trigger function (lines 527-609)
# CodeDeployTriggerLambda - no longer needed

# ❌ Remove EventBridge S3 rule (lines 612-642)
# S3EventRule - no longer needed

# ❌ Remove Lambda execution role (lines 490-525)
# LambdaExecutionRole - no longer needed

# ❌ Remove EventBridge permission (lines 636-642)
# EventBridgeLambdaPermission - no longer needed
```

### Resources to Modify

```yaml
# Update CodeBuild project
CodeBuildProject:
  Properties:
    Source:
      Type: CODEPIPELINE  # Changed from S3
      BuildSpec: buildspec.yml
    # Remove these (CodePipeline manages source):
    # - Location: !Sub '${ArtifactBucket}/sources/placeholder.zip'
```

## Migration Steps

### Phase 1: Preparation (No Downtime)

1. **Move buildspec.yml to root**
   ```bash
   mv config/aws/buildspec.yml buildspec.yml
   git add buildspec.yml
   git commit -m "Move buildspec.yml to root for CodePipeline compatibility"
   ```

2. **Create deploy branches**
   ```bash
   git branch deploy/dev
   git branch deploy/staging
   git branch deploy/prod
   git push origin deploy/dev deploy/staging deploy/prod
   ```

3. **Set up GitHub connection**
   - Create GitHub personal access token (or use CodeStar Connections)
   - Store in AWS Secrets Manager (per env.sh documentation)

4. **Update env.sh**
   ```bash
   # Add GitHub configuration
   echo 'GITHUB_OWNER="your-org"' >> config/env.sh
   echo 'GITHUB_REPO="hello-erlang"' >> config/env.sh
   ```

### Phase 2: Stack Updates (Per Environment)

Start with dev, then staging, then prod:

1. **Update stack parameters**
   ```bash
   ./scripts/aws-stack.sh update dev \
       GitHubOwner="your-org" \
       GitHubRepo="hello-erlang" \
       GitHubBranch="deploy/dev"
   ```

2. **Activate GitHub connection (REQUIRED)**

   After stack deployment, the CodeStar Connection is in **PENDING** state. You must complete the OAuth handshake:

   **Step 2a: Check connection status**
   ```bash
   # Get connection ARN from stack outputs
   CONNECTION_ARN=$(aws cloudformation describe-stacks \
       --stack-name hello-erlang-dev \
       --query 'Stacks[0].Outputs[?OutputKey==`GitHubConnectionArn`].OutputValue' \
       --output text)

   # Check connection status (will show "PENDING")
   aws codestar-connections get-connection --connection-arn "$CONNECTION_ARN"
   ```

   **Step 2b: Complete OAuth handshake in AWS Console**

   1. Navigate to: **AWS Console > Developer Tools > Settings > Connections**
      - Or direct link: https://console.aws.amazon.com/codesuite/settings/connections

   2. You'll see your connection with status "Pending"

   3. Click the connection name, then click **"Update pending connection"**

   4. Click **"Install a new app"** (first time) or select existing installation

   5. You'll be redirected to GitHub:
      - **Authorize the "AWS Connector for GitHub" app**
      - Select which repositories to grant access:
        - Option 1: **"Only select repositories"** (recommended) → Choose `hello-erlang`
        - Option 2: "All repositories" (grants access to everything)
      - Click **"Save"**

   6. You'll be redirected back to AWS Console

   7. Click **"Connect"** to complete the handshake

   8. Connection status changes to **"Available"** ✅

   **Step 2c: Verify connection is active**
   ```bash
   # Check connection status (should now show "AVAILABLE")
   aws codestar-connections get-connection --connection-arn "$CONNECTION_ARN"

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

   **Important notes:**
   - ⚠️  **Pipeline will NOT work** until connection status is "AVAILABLE"
   - 🔒 For **private repositories**: Ensure you grant access to the specific repo
   - 🏢 For **GitHub Organizations**: An org admin must approve the AWS Connector app
   - ✅ This is a **one-time setup** per AWS account/region - subsequent pipelines can reuse the connection
   ```

3. **Verify pipeline created**
   ```bash
   aws codepipeline list-pipelines
   # Should show: dev-hello-erlang-pipeline
   ```

### Phase 3: Testing (Parallel Running)

**Both systems work during this phase**:

1. **Test CodePipeline deployment**
   ```bash
   # Make a trivial change
   echo "# Test deployment" >> README.md
   git add README.md
   git commit -m "Test CodePipeline deployment"
   git push origin main:deploy/dev

   # Watch pipeline
   aws codepipeline get-pipeline-state --name dev-hello-erlang-pipeline
   ```

2. **Verify old script still works**
   ```bash
   ./scripts/aws-deploy.sh build dev
   # Should still work (uses CodeBuild directly)
   ```

3. **Compare results**
   - Both should deploy successfully
   - CodePipeline should be faster (no zip/upload overhead)
   - CloudWatch logs should show both paths

### Phase 4: Cutover (Remove Old System)

Once confident in CodePipeline:

1. **Remove Lambda trigger from stack**
   - Comment out `CodeDeployTriggerLambda` resources in stack.yaml
   - Update stack
   - Lambda no longer triggers deployments from S3

2. **Archive aws-deploy.sh**
   ```bash
   mv scripts/aws-deploy.sh scripts/archive/aws-deploy.sh.old
   git commit -m "Archive manual deployment script (replaced by CodePipeline)"
   ```

3. **Update documentation**
   - Update README.md with new deployment process
   - Add GitOps workflow examples
   - Document branch strategy

### Phase 5: Rollout to Other Environments

Repeat Phase 2-4 for staging and prod:

```bash
# Staging
./scripts/aws-stack.sh update staging GitHubBranch="deploy/staging"

# Prod
./scripts/aws-stack.sh update prod GitHubBranch="deploy/prod"
```

## Deployment Workflow Examples

### Standard Deployment Flow

```bash
# 1. Develop on feature branch
git checkout -b feature/new-endpoint
# ... make changes ...
git commit -m "Add new echo endpoint"

# 2. Merge to main
git checkout main
git merge feature/new-endpoint
git push origin main

# 3. Deploy to dev (automatic via git push)
git push origin main:deploy/dev
# CodePipeline automatically:
#   - Pulls source
#   - Runs CodeBuild (bundles appspec + scripts)
#   - Deploys via CodeDeploy

# 4. Test in dev
./scripts/aws-debug.sh ping dev "test new endpoint"

# 5. Promote to staging
git push origin deploy/dev:deploy/staging

# 6. Validate in staging
./scripts/aws-debug.sh ping staging "validation test"

# 7. Promote to prod
git push origin deploy/staging:deploy/prod
```

### Rollback Scenario

```bash
# Current prod deployment has issues
# Find last good commit
git log deploy/prod

# Rollback to previous commit
git push origin <previous-commit-sha>:deploy/prod --force

# CodePipeline redeploys old version
```

### Hotfix Scenario

```bash
# Critical bug in prod
git checkout deploy/prod
git checkout -b hotfix/critical-bug
# ... fix bug ...
git commit -m "Hotfix: resolve critical issue"

# Deploy hotfix directly to prod
git push origin hotfix/critical-bug:deploy/prod

# Merge back to main
git checkout main
git merge hotfix/critical-bug
git push origin main
```

## Monitoring and Debugging

### Pipeline Status

```bash
# Get pipeline state
aws codepipeline get-pipeline-state \
    --name dev-hello-erlang-pipeline

# Get specific execution
aws codepipeline get-pipeline-execution \
    --pipeline-name dev-hello-erlang-pipeline \
    --pipeline-execution-id <execution-id>
```

### CloudWatch Logs

```bash
# Follow pipeline events
aws logs tail /aws/codepipeline/dev-hello-erlang-pipeline --follow

# Follow build logs
aws logs tail /aws/codebuild/dev-hello-erlang-build --follow

# Follow deployment logs
aws logs tail /aws/codedeploy/dev/deployments --follow
```

### Console Access

```
AWS Console > CodePipeline > dev-hello-erlang-pipeline
    ├─ Visual pipeline view
    ├─ Execution history
    ├─ Stage details
    └─ Direct links to:
        ├─ CodeBuild logs
        ├─ CodeDeploy status
        └─ CloudWatch metrics
```

## Benefits of Migration

### Development Velocity
- ✅ **Faster deployments**: No script overhead (zip, upload)
- ✅ **Git-native workflow**: `git push` instead of custom script
- ✅ **Parallel execution**: Pipeline stages optimized

### Operational Benefits
- ✅ **Visual pipeline**: See status at a glance in console
- ✅ **Integrated logging**: All stages log to CloudWatch
- ✅ **Built-in retries**: Pipeline handles transient failures
- ✅ **Approval gates**: Add manual approval before prod (future)

### GitOps Compliance
- ✅ **Declarative**: Infrastructure state in Git
- ✅ **Auditable**: Git history = deployment history
- ✅ **Reversible**: Rollback via git push
- ✅ **Traceable**: Who deployed what, when, why (commit messages)

## Cost Comparison

### Current (Script-Based)
- CodeBuild: $0.005/min × 5min = $0.025/build
- Lambda triggers: $0.0000002/invocation (negligible)
- S3 storage: ~$0.01/month
- **Total per 100 builds/month**: ~$2.51

### With CodePipeline
- CodePipeline: $1.00/month (first pipeline free, subsequent $1/month)
- CodeBuild: $0.005/min × 0.5min = $0.0025/build (10x faster with optimized image)
- S3 storage: ~$0.02/month (pipeline artifacts)
- **Total per 100 builds/month**: ~$1.27

**Savings**: ~50% cost reduction + 10x faster builds

## FAQs

### Q: Can we still deploy manually in emergencies?
**A**: Yes! CodeBuild and CodeDeploy still exist independently. You can trigger them via AWS CLI if needed.

### Q: What if GitHub is down?
**A**: Use manual deployment as fallback:
```bash
aws codebuild start-build \
    --project-name dev-hello-erlang-build \
    --source-version <commit-sha>
```

### Q: How do we handle secrets in the pipeline?
**A**: Use AWS Secrets Manager. Add to CodeBuild environment variables. (Note: GitHub authentication via CodeStar Connections requires no secrets - AWS handles it automatically!)

### Q: Can we add approval steps before prod?
**A**: Yes! Add manual approval stage:
```yaml
- Name: Approval
  Actions:
    - Name: ManualApproval
      ActionTypeId:
        Category: Approval
        Owner: AWS
        Provider: Manual
        Version: 1
      Configuration:
        CustomData: "Review staging results before prod deployment"
```

### Q: Does this work with the Erlang image optimization?
**A**: Perfectly! CodeBuild stage uses same `CodeBuildProject` resource. All optimizations apply.

### Q: What about multi-region deployments?
**A**: Create separate pipelines per region, each watching the same deploy branch.

## Troubleshooting GitHub Authentication

### Connection Status: PENDING

**Problem**: Pipeline fails with "Connection is not available"

**Solution**: You haven't completed the OAuth handshake yet.
```bash
# Check connection status
aws codestar-connections get-connection --connection-arn "$CONNECTION_ARN"

# If status is "PENDING", go to AWS Console:
# https://console.aws.amazon.com/codesuite/settings/connections
# Complete the OAuth flow (see Phase 2, Step 2 above)
```

### Source Stage Fails: "Repository not found"

**Problem**: Pipeline source stage fails with repository access error

**Possible causes:**
1. **GitHub app not installed on repository**
   - Go to GitHub → Settings → Integrations → Applications
   - Find "AWS Connector for GitHub"
   - Edit repository access → Add the missing repository

2. **Wrong repository name format**
   - Must be: `owner/repo` (e.g., `my-org/hello-erlang`)
   - NOT: `https://github.com/my-org/hello-erlang`

3. **Private repository without access**
   - The AWS Connector app must be granted access to private repos
   - Check GitHub app settings

### Organization Permission Denied

**Problem**: "This organization has not approved the AWS Connector app"

**Solution**:
1. GitHub org admin must approve third-party app
2. Go to: GitHub → Organization → Settings → Third-party access
3. Find "AWS Connector for GitHub" → Grant access
4. Retry connection setup in AWS

### Connection Works But Pipeline Still Fails

**Problem**: Connection status is "AVAILABLE" but source stage fails

**Check these:**
```bash
# 1. Verify branch exists
git ls-remote --heads origin deploy/dev

# 2. Verify CloudFormation parameters
aws cloudformation describe-stacks \
    --stack-name hello-erlang-dev \
    --query 'Stacks[0].Parameters[?ParameterKey==`GitHubBranch`]'

# 3. Check pipeline IAM role has UseConnection permission
aws iam get-role-policy \
    --role-name dev-hello-erlang-pipeline-role \
    --policy-name PipelinePolicy
```

## GitHub Authentication Methods Comparison

| Feature | CodeStar Connections (OAuth) | Personal Access Token |
|---------|----------------------------|----------------------|
| **Setup complexity** | Medium (one-time OAuth) | Low (create token) |
| **Security** | ✅ Excellent (OAuth, scoped) | ⚠️  Poor (static token) |
| **Expiration** | ✅ Never | ❌ 90 days - 1 year |
| **Maintenance** | ✅ None | ❌ Manual rotation |
| **Private repos** | ✅ Yes | ✅ Yes |
| **Org visibility** | ✅ Visible to admins | ❌ Hidden (personal token) |
| **Revocation** | ✅ Instant (AWS or GitHub) | ⚠️  Must rotate everywhere |
| **CloudFormation** | ✅ Native resource | ⚠️  Via Secrets Manager |
| **AWS recommendation** | ✅ Preferred | ❌ Deprecated |
| **GitHub recommendation** | ✅ GitHub Apps | ❌ Legacy |
| **Audit trail** | ✅ AWS CloudTrail | ⚠️  Limited |
| **Cost** | ✅ Free | ✅ Free |
| **Best for** | **Production** | Testing only |

**Recommendation**: Use CodeStar Connections for all production pipelines.

## Next Steps

1. Review this document with team
2. Decide on git branch naming convention
3. **Set up CodeStar Connections** (recommended - see Phase 2)
4. Start with dev environment migration
5. Run parallel (script + pipeline) for 1-2 weeks
6. Cutover remaining environments
7. Archive old deployment script

## Legacy Authentication Methods

### Personal Access Tokens (For Reference Only)

**⚠️  This section is included only for completeness. Use CodeStar Connections instead.**

If you absolutely must use PATs (e.g., GitHub Enterprise Server on-premises):

**1. Create GitHub Personal Access Token**
- GitHub → Settings → Developer settings → Personal access tokens → Tokens (classic)
- Generate new token (classic)
- Scopes: `repo` (private) or `public_repo` (public only)
- Expiration: Maximum allowed (but you'll need to rotate it)

**2. Store in AWS Secrets Manager**
```bash
aws secretsmanager create-secret \
    --name github-personal-access-token \
    --description "GitHub PAT for CodePipeline (LEGACY)" \
    --secret-string "ghp_your_token_here"
```

**3. Use in CloudFormation (OLD v1 Provider)**
```yaml
# NOT RECOMMENDED - Use CodeStarConnections instead
SourceAction:
  ActionTypeId:
    Category: Source
    Owner: ThirdParty
    Provider: GitHub  # v1 provider (deprecated)
    Version: 1
  Configuration:
    Owner: !Ref GitHubOwner
    Repo: !Ref GitHubRepo
    Branch: !Ref GitHubBranch
    OAuthToken: !Sub '{{resolve:secretsmanager:github-personal-access-token:SecretString}}'
  OutputArtifacts:
    - Name: SourceOutput
```

**4. Set up rotation reminder**
```bash
# PATs expire - set a calendar reminder to rotate before expiration
# This is why CodeStar Connections is better - no rotation needed
```

## Related Documents
- [CodeBuild Optimization Plan](./codebuild-optimization.md) - Reduce build time from 5min to 30sec
- [Logging Strategy](./logging-strategy.md) - Current CloudWatch logging setup
