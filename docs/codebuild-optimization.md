# CodeBuild Optimization Plan

## Problem Statement

### Current State
Our CodeBuild process currently rebuilds Erlang/OTP from source on every build using `kerl`:
- **Build time**: ~5 minutes (Erlang compilation: ~4.5 min, Application build: ~30 sec)
- **Inefficiency**: Erlang version (27.1) rarely changes, but gets rebuilt every time
- **Location**: `config/aws/buildspec.yml` lines 11-34

### Impact
- Slow feedback loop for developers
- Increased AWS costs (5min vs 30sec build time = 10x cost)
- Unnecessary resource consumption

## Recommended Solution: Custom Docker Image

### Overview
Pre-build a custom Docker image with Erlang/OTP and rebar3 installed, push to Amazon ECR, and configure CodeBuild to use it.

### Benefits
- ✅ **Build time reduction**: 5 minutes → 30 seconds (10x faster)
- ✅ **Cost savings**: ~90% reduction in CodeBuild costs
- ✅ **Simplicity**: No conditional logic or multi-stage builds needed
- ✅ **Minimal changes**: Only update CloudFormation image parameter
- ✅ **Version control**: Tag images by Erlang version (e.g., `erlang-27.1`)
- ✅ **Reusability**: Same image can be used across multiple projects

### Architecture
```
┌─────────────────────┐
│   ECR Repository    │
│  erlang-codebuild   │
│                     │
│  Tags:              │
│  - 27.1 (current)   │
│  - 26.2.5           │
│  - latest           │
└─────────────────────┘
          ↓
┌─────────────────────┐
│   CodeBuild         │
│   (uses image)      │
│                     │
│  Build time: 30s    │
└─────────────────────┘
```

## Implementation Plan

### Phase 1: Create Docker Image

#### Step 1.1: Create Dockerfile
**File**: `config/aws/Dockerfile.erlang-build`

```dockerfile
FROM aws/codebuild/amazonlinux2-x86_64-standard:5.0

# Install build dependencies
RUN yum install -y \
    wget \
    tar \
    gzip \
    git \
    ncurses-devel \
    openssl-devel

# Install kerl
RUN curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl && \
    chmod +x kerl && \
    mv kerl /usr/local/bin/

# Build and install Erlang/OTP
ARG ERLANG_VERSION=27.1
ENV KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --without-wx --without-odbc"
ENV KERL_BUILD_DOCS=no

RUN kerl build ${ERLANG_VERSION} ${ERLANG_VERSION} && \
    kerl install ${ERLANG_VERSION} /usr/local/erlang && \
    kerl cleanup ${ERLANG_VERSION}

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Set up environment
RUN echo 'source /usr/local/erlang/activate' >> /etc/profile.d/erlang.sh
ENV PATH="/usr/local/erlang/bin:${PATH}"

# Verify installation
RUN erl -version && rebar3 version

# Add labels for documentation
LABEL maintainer="your-team@example.com"
LABEL erlang.version="${ERLANG_VERSION}"
LABEL description="CodeBuild image with Erlang/OTP pre-installed"
```

#### Step 1.2: Create Build and Push Script
**File**: `scripts/build-erlang-image.sh`

```bash
#!/bin/bash
# Build and push Erlang CodeBuild image to ECR

set -e

cd "$(dirname "$0")/.."

# Configuration
ERLANG_VERSION="${1:-27.1}"
AWS_REGION="${AWS_REGION:-us-east-1}"
AWS_ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
ECR_REPO_NAME="erlang-codebuild"
IMAGE_TAG="${ERLANG_VERSION}"
ECR_URI="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}"

usage() {
    echo "Usage: $0 [erlang-version]"
    echo ""
    echo "Build and push Erlang CodeBuild Docker image to ECR"
    echo ""
    echo "Arguments:"
    echo "  erlang-version    Erlang/OTP version to install (default: 27.1)"
    echo ""
    echo "Examples:"
    echo "  $0              # Build with Erlang 27.1"
    echo "  $0 26.2.5       # Build with Erlang 26.2.5"
    echo ""
    echo "Prerequisites:"
    echo "  - AWS CLI configured with appropriate credentials"
    echo "  - Docker installed and running"
    echo "  - ECR repository '${ECR_REPO_NAME}' must exist"
    exit 1
}

if [ "$1" == "--help" ] || [ "$1" == "-h" ]; then
    usage
fi

echo "=== Building Erlang CodeBuild Image ==="
echo "Erlang Version: ${ERLANG_VERSION}"
echo "ECR Repository: ${ECR_URI}"
echo "Image Tag: ${IMAGE_TAG}"
echo ""

# Check if ECR repository exists
echo "Checking ECR repository..."
if ! aws ecr describe-repositories --repository-names "${ECR_REPO_NAME}" --region "${AWS_REGION}" >/dev/null 2>&1; then
    echo "ECR repository '${ECR_REPO_NAME}' does not exist. Creating..."
    aws ecr create-repository \
        --repository-name "${ECR_REPO_NAME}" \
        --region "${AWS_REGION}" \
        --image-scanning-configuration scanOnPush=true \
        --tags Key=Application,Value=hello-erlang Key=Purpose,Value=codebuild-base-image
    echo "✓ Repository created"
else
    echo "✓ Repository exists"
fi

# Build Docker image
echo ""
echo "Building Docker image..."
docker build \
    --build-arg ERLANG_VERSION="${ERLANG_VERSION}" \
    -t "${ECR_REPO_NAME}:${IMAGE_TAG}" \
    -t "${ECR_REPO_NAME}:latest" \
    -f config/aws/Dockerfile.erlang-build \
    .

echo "✓ Image built successfully"

# Login to ECR
echo ""
echo "Logging in to ECR..."
aws ecr get-login-password --region "${AWS_REGION}" | \
    docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"
echo "✓ Logged in to ECR"

# Tag and push image
echo ""
echo "Pushing image to ECR..."
docker tag "${ECR_REPO_NAME}:${IMAGE_TAG}" "${ECR_URI}:${IMAGE_TAG}"
docker tag "${ECR_REPO_NAME}:latest" "${ECR_URI}:latest"

docker push "${ECR_URI}:${IMAGE_TAG}"
docker push "${ECR_URI}:latest"

echo "✓ Image pushed successfully"
echo ""
echo "=== Build Complete ==="
echo "Image URI: ${ECR_URI}:${IMAGE_TAG}"
echo ""
echo "Next steps:"
echo "  1. Update CloudFormation stack parameter 'CodeBuildImage'"
echo "  2. Or manually update CodeBuild project to use: ${ECR_URI}:${IMAGE_TAG}"
```

#### Step 1.3: Make Script Executable
```bash
chmod +x scripts/build-erlang-image.sh
```

### Phase 2: Update CloudFormation Stack

#### Step 2.1: Add ECR Image Parameter
**File**: `config/aws/stack.yaml`

Add after line 38 (after `ErlangVersion` parameter):

```yaml
  CodeBuildImage:
    Type: String
    Default: 'aws/codebuild/amazonlinux2-x86_64-standard:5.0'
    Description: Docker image for CodeBuild (use ECR URI for custom Erlang image)
```

#### Step 2.2: Update CodeBuild Project
**File**: `config/aws/stack.yaml`

Replace line 177:
```yaml
# OLD
Image: aws/codebuild/amazonlinux2-x86_64-standard:5.0

# NEW
Image: !Ref CodeBuildImage
```

#### Step 2.3: Add ECR Pull Permissions
**File**: `config/aws/stack.yaml`

Add to `CodeBuildRole` policies (after line 153):

```yaml
              # ECR access for custom build images
              - Effect: Allow
                Action:
                  - ecr:GetAuthorizationToken
                  - ecr:BatchCheckLayerAvailability
                  - ecr:GetDownloadUrlForLayer
                  - ecr:BatchGetImage
                Resource: '*'
```

### Phase 3: Simplify buildspec.yml

#### Step 3.1: Update buildspec.yml
**File**: `config/aws/buildspec.yml`

Replace the entire `install` phase (lines 7-34) with:

```yaml
  install:
    runtime-versions:
      python: 3.11
    commands:
      - echo "Activating pre-installed Erlang/OTP..."
      - source /usr/local/erlang/activate
      - export PATH=/usr/local/erlang/bin:$PATH
      - erl -version
      - rebar3 version
```

### Phase 4: Deployment

#### Step 4.1: Build and Push Image
```bash
# Build image with default Erlang version (27.1)
./scripts/build-erlang-image.sh

# Or specify a different version
./scripts/build-erlang-image.sh 26.2.5
```

#### Step 4.2: Update Stack
```bash
# Get ECR image URI
ECR_IMAGE=$(aws ecr describe-repositories \
    --repository-names erlang-codebuild \
    --query 'repositories[0].repositoryUri' \
    --output text):27.1

# Update stack with new parameter
./scripts/aws-stack.sh update dev \
    CodeBuildImage="${ECR_IMAGE}"
```

#### Step 4.3: Test Build
```bash
# Trigger a build
./scripts/aws-deploy.sh build dev

# Build should complete in ~30 seconds instead of ~5 minutes
```

## Expected Results

### Before
```
[Container] Phase: INSTALL
[Container] Installing Erlang/OTP 27.1...
[Container] (4.5 minutes of kerl compilation)
[Container] Phase: BUILD
[Container] Building Erlang release...
[Container] (30 seconds)
Total: ~5 minutes
```

### After
```
[Container] Phase: INSTALL
[Container] Activating pre-installed Erlang/OTP...
[Container] (2 seconds)
[Container] Phase: BUILD
[Container] Building Erlang release...
[Container] (30 seconds)
Total: ~32 seconds
```

## Maintenance

### Updating Erlang Version
When you need to upgrade Erlang:

1. Build new image:
   ```bash
   ./scripts/build-erlang-image.sh 27.2
   ```

2. Update stack parameter:
   ```bash
   ./scripts/aws-stack.sh update dev \
       CodeBuildImage="<account>.dkr.ecr.<region>.amazonaws.com/erlang-codebuild:27.2"
   ```

3. Both tags are pushed:
   - `27.2` - specific version
   - `latest` - always points to most recent

### Image Management
- **Lifecycle**: Set ECR lifecycle policy to keep last 3 images
- **Scanning**: Enabled automatically in build script
- **Cost**: Minimal (~$0.10/month per image in ECR)

## Alternative Approaches (Not Recommended)

### Option 2: CodeBuild S3 Cache
**Pros**: No Docker image management
**Cons**:
- Still requires ~1-2 min for cache restore
- Less reliable (cache can be invalidated)
- More complex buildspec logic

### Option 3: Two-Stage CodeBuild
**Pros**: Fully automated base image builds
**Cons**:
- Much more complex (2 projects, conditional logic)
- Overkill for this use case
- Harder to debug

### Option 4: Multi-Repo with Pulumi
**Cons**:
- Unnecessary complexity
- CloudFormation handles this fine
- Added toolchain dependency

## Migration to CodePipeline

This optimization is **fully compatible** with CodePipeline migration:

```yaml
# CodePipeline Build stage will use the same custom image
BuildStage:
  - Name: Build
    Actions:
      - Name: BuildApplication
        ActionTypeId:
          Category: Build
          Owner: AWS
          Provider: CodeBuild
          Version: 1
        Configuration:
          ProjectName: !Ref CodeBuildProject  # Already uses custom image
```

No additional changes needed when adding CodePipeline.

## Cost-Benefit Analysis

### Current Costs (per build)
- Build time: 5 minutes
- CodeBuild rate: $0.005/min (general1.large)
- Cost per build: $0.025
- 100 builds/month: **$2.50**

### Optimized Costs (per build)
- Build time: 30 seconds
- CodeBuild rate: $0.005/min (general1.large)
- Cost per build: $0.0025
- 100 builds/month: **$0.25**
- ECR storage: **$0.10/month**

**Monthly savings**: $2.15 (86% reduction)
**Annual savings**: $25.80

### Non-Monetary Benefits
- ✅ Faster developer feedback (10x improvement)
- ✅ More frequent deployments possible
- ✅ Better CI/CD experience
- ✅ Reduced AWS resource consumption

## Implementation Checklist

- [ ] Create `config/aws/Dockerfile.erlang-build`
- [ ] Create `scripts/build-erlang-image.sh`
- [ ] Make script executable
- [ ] Build and push initial image to ECR
- [ ] Add `CodeBuildImage` parameter to stack.yaml
- [ ] Update CodeBuild project to use parameter
- [ ] Add ECR permissions to CodeBuildRole
- [ ] Simplify buildspec.yml install phase
- [ ] Update stack with new parameter value
- [ ] Test build with custom image
- [ ] Document ECR image URI in team wiki
- [ ] Set up ECR lifecycle policy (optional)
- [ ] Update this document with actual ECR URI

## Questions?

- **Q**: What if we need multiple Erlang versions?
  - **A**: Build multiple images with different tags, specify at stack creation

- **Q**: How do we handle security patches?
  - **A**: Rebuild image periodically (quarterly), push with new date tag

- **Q**: Can we use this for local development?
  - **A**: Yes! Pull ECR image and use for local builds too

- **Q**: What about rebar3 updates?
  - **A**: Rebuild image with new rebar3 version when needed
