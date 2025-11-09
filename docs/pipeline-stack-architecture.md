# Pipeline and Application Stack Architecture

## The Question

Should we:
1. **Keep current approach**: One stack per environment (dev, staging, prod), each with its own pipeline
2. **Split stacks**: Separate pipeline infrastructure from application infrastructure

## Current Architecture (Unified Stacks)

```
Stack: hello-erlang-dev (Parameters: Environment=dev)
├── Pipeline Resources (future)
│   ├── CodePipeline
│   └── GitHub Connection
├── Build Resources
│   ├── CodeBuild Project
│   └── S3 Artifact Bucket
├── Deploy Resources
│   ├── CodeDeploy Application
│   └── CodeDeploy Deployment Group
└── Application Resources
    ├── EC2 Instance
    ├── ALB + Target Group
    └── Security Groups

Stack: hello-erlang-staging (Parameters: Environment=staging)
├── (Same structure, different resources)

Stack: hello-erlang-prod (Parameters: Environment=prod)
├── (Same structure, different resources)
```

**Commands**:
```bash
# Create all environments
./scripts/aws-stack.sh create dev InstanceType=t3.medium
./scripts/aws-stack.sh create staging InstanceType=t3.medium
./scripts/aws-stack.sh create prod InstanceType=t3.large

# Each stack is completely independent
```

## Option 1: Keep Unified Stacks (RECOMMENDED)

### Architecture

**One CloudFormation stack per environment**, containing everything:

```
hello-erlang-dev:
  CodePipeline (watches deploy/dev branch)
  CodeBuild
  CodeDeploy
  EC2 + ALB

hello-erlang-staging:
  CodePipeline (watches deploy/staging branch)
  CodeBuild
  CodeDeploy
  EC2 + ALB

hello-erlang-prod:
  CodePipeline (watches deploy/prod branch)
  CodeBuild
  CodeDeploy
  EC2 + ALB
```

### GitOps Workflow

```bash
# Deploy to dev
git push origin main:deploy/dev
→ Triggers hello-erlang-dev pipeline
→ Builds in hello-erlang-dev CodeBuild
→ Deploys to hello-erlang-dev EC2

# Deploy to staging
git push origin deploy/dev:deploy/staging
→ Triggers hello-erlang-staging pipeline
→ Builds in hello-erlang-staging CodeBuild
→ Deploys to hello-erlang-staging EC2

# Deploy to prod
git push origin deploy/staging:deploy/prod
→ Triggers hello-erlang-prod pipeline
→ Builds in hello-erlang-prod CodeBuild
→ Deploys to hello-erlang-prod EC2
```

### Pros

✅ **Simple mental model**: One stack = one environment = one complete system

✅ **Complete isolation**: Dev pipeline failure doesn't affect staging/prod

✅ **Independent lifecycle**: Can update dev stack without touching prod

✅ **Easy to create/destroy**: Delete entire environment with one command
```bash
./scripts/aws-stack.sh delete dev  # Everything gone
```

✅ **Clear ownership**: Each stack has its own IAM roles, buckets, logs

✅ **Parallel deployments**: Dev and staging can deploy simultaneously without conflicts

✅ **Environment-specific configuration**: Different instance types, different alerting thresholds
```bash
./scripts/aws-stack.sh create dev InstanceType=t3.medium SlackWebhook=dev-channel
./scripts/aws-stack.sh create prod InstanceType=c7i.xlarge SlackWebhook=prod-channel
```

✅ **Easier debugging**: All resources for one environment grouped together in console

✅ **No cross-stack dependencies**: No risk of circular dependencies or update ordering issues

### Cons

❌ **Resource duplication**: 3 CodeBuild projects, 3 S3 buckets (but each is small)

❌ **More CloudFormation stacks**: 3 stacks instead of 4 (not really a problem)

❌ **Slightly higher cost**: ~$10-20/month more (negligible)

### Cost Analysis

```
Dev Stack:
- EC2 t3.medium: $30/mo
- S3 bucket: $1/mo
- CodeBuild (on-demand): $0/mo (only when building)
- CodePipeline: FREE (first pipeline)
Total: ~$31/mo

Staging Stack:
- EC2 t3.medium: $30/mo
- S3 bucket: $1/mo
- CodeBuild (on-demand): $0/mo
- CodePipeline: $1/mo
Total: ~$32/mo

Prod Stack:
- EC2 c7i.xlarge: $130/mo
- S3 bucket: $1/mo
- CodeBuild (on-demand): $2/mo (more frequent)
- CodePipeline: $1/mo
Total: ~$134/mo

TOTAL: ~$197/mo
```

## Option 2: Split Pipeline and Application Stacks

### Architecture

**One pipeline stack handling all environments**:

```
hello-erlang-pipeline:
  CodePipeline Dev (watches deploy/dev)
  CodePipeline Staging (watches deploy/staging)
  CodePipeline Prod (watches deploy/prod)
  CodeBuild (shared)
  S3 Artifact Bucket (shared)

hello-erlang-app-dev:
  CodeDeploy Application
  CodeDeploy Deployment Group
  EC2 + ALB

hello-erlang-app-staging:
  CodeDeploy Application
  CodeDeploy Deployment Group
  EC2 + ALB

hello-erlang-app-prod:
  CodeDeploy Application
  CodeDeploy Deployment Group
  EC2 + ALB
```

### Cross-Stack References

```yaml
# In pipeline stack
Outputs:
  ArtifactBucketName:
    Value: !Ref ArtifactBucket
    Export:
      Name: hello-erlang-artifact-bucket

# In application stacks
Parameters:
  ArtifactBucket:
    Type: String
    Default: !ImportValue hello-erlang-artifact-bucket
```

### Pros

✅ **Single pipeline to manage**: Update pipeline code in one place

✅ **Shared build infrastructure**: One CodeBuild project, one artifact bucket

✅ **Centralized pipeline visibility**: See all environments in one place

✅ **Slightly lower cost**: ~$10/month savings (shared resources)

✅ **DRY principle**: Pipeline logic not duplicated

### Cons

❌ **Cross-stack dependencies**: Application stacks depend on pipeline stack
```bash
# Can't delete pipeline stack without deleting app stacks first
# Can't update pipeline without potentially affecting all environments
```

❌ **Blast radius**: Pipeline stack issue affects all environments

❌ **Complex update ordering**: Must update pipeline stack before app stacks

❌ **Harder to reason about**: Resources split across multiple stacks

❌ **IAM complexity**: Pipeline needs permissions to deploy to all environments

❌ **Shared CodeBuild**: Dev builds could block prod builds (queue delays)

❌ **Harder to isolate**: Can't easily spin up a temporary test environment

❌ **No parallel builds**: Shared CodeBuild means sequential builds
```bash
# If dev and staging both push at same time:
git push origin main:deploy/dev       # Build 1 starts
git push origin main:deploy/staging   # Build 2 queued (waits)
```

❌ **Artifact bucket conflicts**: All environments share bucket (namespace collisions possible)

❌ **More complex setup**: Need to create stacks in specific order
```bash
# Must do this:
./scripts/aws-stack.sh create-pipeline
./scripts/aws-stack.sh create-app dev
./scripts/aws-stack.sh create-app staging
./scripts/aws-stack.sh create-app prod

# Can't do:
./scripts/aws-stack.sh create dev  # One command
```

### Cost Analysis

```
Pipeline Stack (shared):
- S3 bucket: $2/mo (all artifacts)
- CodeBuild (on-demand): $2/mo
- CodePipeline × 3: $2/mo (first free, $1 each for 2 more)
Total: ~$6/mo

App Stack Dev:
- EC2 t3.medium: $30/mo
- CodeDeploy: FREE
Total: ~$30/mo

App Stack Staging:
- EC2 t3.medium: $30/mo
- CodeDeploy: FREE
Total: ~$30/mo

App Stack Prod:
- EC2 c7i.xlarge: $130/mo
- CodeDeploy: FREE
Total: ~$130/mo

TOTAL: ~$196/mo (saves ~$1/mo)
```

**Savings: $1/month** (not worth the complexity)

## Option 3: Hybrid (Middle Ground)

**Separate build infrastructure, keep pipeline per environment**:

```
hello-erlang-build:
  CodeBuild Project (shared)
  S3 Artifact Bucket (shared)

hello-erlang-dev:
  CodePipeline (uses shared CodeBuild)
  CodeDeploy
  EC2 + ALB

hello-erlang-staging:
  CodePipeline (uses shared CodeBuild)
  CodeDeploy
  EC2 + ALB

hello-erlang-prod:
  CodePipeline (uses shared CodeBuild)
  CodeDeploy
  EC2 + ALB
```

### Pros

✅ **Isolated pipelines**: Each environment has its own orchestration

✅ **Shared build**: One CodeBuild project, saving duplication

✅ **Parallel builds possible**: CodeBuild can run multiple builds concurrently

### Cons

❌ **Still has cross-stack dependencies**: Apps depend on build stack

❌ **Complex**: More stacks than Option 1, more complexity than Option 2

❌ **Shared CodeBuild still a bottleneck**: Under heavy load, builds queue

❌ **Marginal cost savings**: ~$5/month (not worth it)

## Comparison Matrix

| Aspect | Option 1 (Unified) | Option 2 (Split) | Option 3 (Hybrid) |
|--------|-------------------|------------------|-------------------|
| **Stacks** | 3 (dev, staging, prod) | 4 (pipeline + 3 apps) | 4 (build + 3 apps) |
| **Complexity** | Low | High | Medium |
| **Isolation** | Complete | Partial | Partial |
| **Cost** | ~$197/mo | ~$196/mo | ~$192/mo |
| **Parallel Deploys** | ✅ Yes | ✅ Yes | ✅ Yes |
| **Parallel Builds** | ✅ Yes | ❌ No | ⚠️ Limited |
| **Setup Steps** | 3 (one per env) | 4 (pipeline + 3 apps) | 4 (build + 3 apps) |
| **Cross-stack Deps** | None | Many | Some |
| **Easy to Delete** | ✅ Yes | ❌ No (order matters) | ❌ No (order matters) |
| **Update Blast Radius** | Minimal | High | Medium |
| **Mental Model** | ✅ Simple | ❌ Complex | ⚠️ Medium |

## Real-World Patterns

### What Companies Do

**Startups/Small Teams** (< 20 engineers):
- **Unified stacks** (Option 1)
- Reason: Simplicity, easy to understand, low overhead

**Mid-size Companies** (20-100 engineers):
- **Unified stacks** (Option 1)
- Some split for cost optimization, but often regret it

**Large Companies** (100+ engineers):
- **Split stacks** (Option 2)
- Multiple teams, centralized platform team manages pipelines
- Cost savings at scale (100s of apps)

### Your Context

- Small team (< 5 engineers?)
- Single application
- 3 environments
- Cost-conscious

**Best fit: Option 1 (Unified Stacks)**

## Recommendation: Keep Unified Stacks (Option 1)

### Why This is the Right Choice

**1. Simplicity**
```bash
# Everything in one place
aws cloudformation describe-stacks --stack-name hello-erlang-dev
# Shows: Pipeline, Build, Deploy, EC2, ALB - complete picture
```

**2. Independence**
```bash
# Experiment in dev without risk
./scripts/aws-stack.sh update dev NewParameter=test
# Prod is completely unaffected
```

**3. Easy Cleanup**
```bash
# Delete entire temporary environment
./scripts/aws-stack.sh delete feature-test
# No dangling resources, no dependencies to worry about
```

**4. Clear Ownership**
```
Dev environment broke? → Check hello-erlang-dev stack
Staging deployment failed? → Check hello-erlang-staging stack
Prod needs scaling? → Update hello-erlang-prod stack

No confusion about which stack owns what.
```

**5. Future-Proof**
```bash
# Want to add a temporary demo environment?
./scripts/aws-stack.sh create demo InstanceType=t3.small
# Done. Completely isolated.

# Want to test infrastructure changes?
./scripts/aws-stack.sh create infra-test InstanceType=t3.micro
# Test without affecting any existing environment

# Done with it?
./scripts/aws-stack.sh delete infra-test
# Clean slate.
```

**6. Parallel Operations**
```bash
# These can happen simultaneously:
git push origin main:deploy/dev      # Dev pipeline starts
git push origin main:deploy/staging  # Staging pipeline starts

# Both build in parallel (separate CodeBuild projects)
# No queueing, no blocking
```

**7. Environment-Specific Configuration**

```yaml
# Dev: Small instance, frequent builds, verbose logging
./scripts/aws-stack.sh create dev \
    InstanceType=t3.medium \
    LogRetentionDays=3 \
    AlertThreshold=90

# Prod: Large instance, infrequent builds, long retention
./scripts/aws-stack.sh create prod \
    InstanceType=c7i.xlarge \
    LogRetentionDays=30 \
    AlertThreshold=70
```

## Migration Path with CodePipeline

### Current State (No Pipeline)

```
hello-erlang-dev (stack):
  - CodeBuild
  - CodeDeploy
  - EC2 + ALB
  - Lambda trigger (S3 → CodeDeploy)
```

### After CodePipeline Migration (Unified)

```
hello-erlang-dev (stack):
  - CodePipeline (NEW - watches deploy/dev)
  - CodeBuild (existing, now triggered by pipeline)
  - CodeDeploy (existing, now triggered by pipeline)
  - EC2 + ALB (existing, unchanged)
  - (Remove: Lambda trigger - replaced by pipeline)
```

**Changes to stack.yaml**:
- Add: `CodePipeline` resource
- Add: `GitHubConnection` resource
- Add: `PipelineServiceRole` resource
- Modify: `CodeBuildProject` (source: S3 → CODEPIPELINE)
- Remove: `CodeDeployTriggerLambda`
- Remove: `S3EventRule`
- Remove: `LambdaExecutionRole`

**Stack update command**:
```bash
./scripts/aws-stack.sh update dev \
    GitHubOwner=your-org \
    GitHubRepo=hello-erlang \
    GitHubBranch=deploy/dev

# Repeat for staging and prod
```

### No Need to Split

**Why unified stays simple**:
```bash
# Before CodePipeline
./scripts/aws-deploy.sh build dev

# After CodePipeline
git push origin main:deploy/dev

# Same mental model: one command affects one environment
```

## Alternative: When to Consider Split Stacks

### Scenarios Where Split Makes Sense

**1. Multiple Applications Sharing Pipeline**
```
pipeline-infrastructure:
  CodeBuild (shared)
  S3 Artifacts (shared)

app1-dev, app1-staging, app1-prod
app2-dev, app2-staging, app2-prod
app3-dev, app3-staging, app3-prod

# 1 pipeline stack + 9 app stacks
# Saves: ~$200/month (9 CodeBuild projects → 1 shared)
```

**2. Centralized Platform Team**
```
Team Structure:
- Platform team: Manages pipeline stack
- App teams: Manage their own app stacks

Benefit: Clear separation of responsibility
```

**3. Strict Cost Optimization**
```
100+ environments
Shared build infrastructure
Savings: ~$1000s/month
```

### Your Situation

- ✅ Single application
- ✅ Small team (everyone touches everything)
- ✅ 3 environments (dev, staging, prod)

**Verdict: Unified stacks are better for you.**

## Implementation Recommendation

### Current Setup (Keep This Structure)

```bash
# Create environments independently
./scripts/aws-stack.sh create dev \
    Environment=dev \
    InstanceType=t3.medium \
    VpcId=vpc-xxx \
    ALBSubnets=subnet-a,subnet-b

./scripts/aws-stack.sh create staging \
    Environment=staging \
    InstanceType=t3.medium \
    VpcId=vpc-xxx \
    ALBSubnets=subnet-a,subnet-b

./scripts/aws-stack.sh create prod \
    Environment=prod \
    InstanceType=c7i.xlarge \
    VpcId=vpc-xxx \
    ALBSubnets=subnet-a,subnet-b
```

### Adding CodePipeline (Extend Each Stack)

**Update stack.yaml** to add CodePipeline resources to the existing template:

```yaml
Parameters:
  # ... existing parameters ...
  GitHubOwner:
    Type: String
    Description: GitHub repository owner
  GitHubRepo:
    Type: String
    Default: hello-erlang
    Description: GitHub repository name
  GitHubBranch:
    Type: String
    Description: Branch to monitor (deploy/dev, deploy/staging, deploy/prod)

Resources:
  # ... existing resources (EC2, ALB, CodeBuild, etc.) ...

  # NEW: CodePipeline resources
  GitHubConnection:
    Type: AWS::CodeStarConnections::Connection
    Properties:
      ConnectionName: !Sub '${Environment}-hello-erlang-github'
      ProviderType: GitHub

  CodePipeline:
    Type: AWS::CodePipeline::Pipeline
    Properties:
      Name: !Sub '${Environment}-hello-erlang-pipeline'
      RoleArn: !GetAtt CodePipelineServiceRole.Arn
      Stages:
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
              OutputArtifacts:
                - Name: SourceOutput
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
```

**Update each environment**:
```bash
./scripts/aws-stack.sh update dev \
    GitHubOwner=your-org \
    GitHubBranch=deploy/dev

./scripts/aws-stack.sh update staging \
    GitHubOwner=your-org \
    GitHubBranch=deploy/staging

./scripts/aws-stack.sh update prod \
    GitHubOwner=your-org \
    GitHubBranch=deploy/prod
```

**Result**: Three complete, independent stacks, each with their own pipeline.

## Handling the "More Work" Concern

### You Said: "More work because we need to handle two separate stacks"

**Counter-argument**: Unified stacks are actually LESS work:

**With Unified (3 stacks)**:
```bash
# Create
./scripts/aws-stack.sh create dev
./scripts/aws-stack.sh create staging
./scripts/aws-stack.sh create prod

# Update
./scripts/aws-stack.sh update dev SomeParam=value
./scripts/aws-stack.sh update staging SomeParam=value
./scripts/aws-stack.sh update prod SomeParam=value

# Delete
./scripts/aws-stack.sh delete dev

# Total: 3 stacks, 3 commands each
```

**With Split (4+ stacks)**:
```bash
# Create (order matters!)
./scripts/aws-stack.sh create-pipeline
./scripts/aws-stack.sh create-app dev
./scripts/aws-stack.sh create-app staging
./scripts/aws-stack.sh create-app prod

# Update (must update pipeline first, then apps)
./scripts/aws-stack.sh update-pipeline SomeParam=value
./scripts/aws-stack.sh update-app dev SomeParam=value
./scripts/aws-stack.sh update-app staging SomeParam=value
./scripts/aws-stack.sh update-app prod SomeParam=value

# Delete (order matters!)
./scripts/aws-stack.sh delete-app dev
./scripts/aws-stack.sh delete-app staging
./scripts/aws-stack.sh delete-app prod
./scripts/aws-stack.sh delete-pipeline

# Total: 4 stacks, 4 commands each, PLUS ordering constraints
```

**Unified is less work, not more.**

## Conclusion

### Your Question: "What is the best way forward?"

**Answer: Keep unified stacks (Option 1)**

**Reasons**:
1. ✅ Simpler (no cross-stack dependencies)
2. ✅ More isolated (blast radius contained)
3. ✅ Easier to operate (one command per environment)
4. ✅ Easier to understand (one stack = one environment)
5. ✅ Better for small teams (less cognitive overhead)
6. ✅ Parallel builds (no shared bottlenecks)
7. ✅ Cost difference negligible (~$1/month savings not worth complexity)

**Your intuition is correct**: "Seems more elegant but also more work" → it IS more work, and NOT more elegant for your use case.

### Implementation Steps

1. **Keep current stack structure** (one stack per environment)
2. **Add CodePipeline resources** to existing stack.yaml
3. **Update each stack** with GitHub parameters
4. **No need to split** - unified stacks work seamlessly with CodePipeline

### GitOps Flow (After Migration)

```bash
# Deploy to dev
git push origin main:deploy/dev
→ hello-erlang-dev pipeline triggers
→ Builds in hello-erlang-dev CodeBuild
→ Deploys to hello-erlang-dev EC2

# Promote to staging
git push origin deploy/dev:deploy/staging
→ hello-erlang-staging pipeline triggers
→ Builds in hello-erlang-staging CodeBuild
→ Deploys to hello-erlang-staging EC2

# Promote to prod
git push origin deploy/staging:deploy/prod
→ hello-erlang-prod pipeline triggers
→ Builds in hello-erlang-prod CodeBuild
→ Deploys to hello-erlang-prod EC2
```

**Simple. Clear. Isolated. Correct.**

---

## Appendix: When to Reconsider

You should reconsider split stacks if:

1. You have **10+ applications** sharing infrastructure
2. You have a **dedicated platform team** (5+ engineers)
3. You need **strict cost optimization** at scale (100+ environments)
4. You have **compliance requirements** for centralized pipeline management

**For now: unified stacks are the right choice.**
