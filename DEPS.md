# Dependencies

This project has external dependencies that must be set up before deployment.

## Critical Dependency: Erlang CodeBuild Images

**Location:** `/Users/jhw/work/gists/aws/8aafb8a1796b06d94ea8178beeddc9d7`

### What It Does

The CodeBuild stage of this project requires pre-built Docker images containing Erlang/OTP and rebar3. These images are created and stored in Amazon ECR by a separate infrastructure project.

### Why It's Needed

Building Erlang/OTP from source with kerl takes 4-5 minutes on every CodeBuild run. By using pre-built images, build times drop from ~5 minutes to ~30 seconds (10x faster).

### Project Reference

The Erlang image builder project provides:
- CloudFormation stack that creates an ECR repository (`erlang-codebuild`)
- CodeBuild project for building Erlang Docker images
- Scripts for building and managing images for different Erlang versions

### How This Project Uses It

**In `config/aws/stack.yaml` (line 199):**
```yaml
Environment:
  Image: !Sub '${AWS::AccountId}.dkr.ecr.${AWS::Region}.amazonaws.com/erlang-codebuild:${ErlangVersion}'
```

**In `config/aws/buildspec.yml`:**
```yaml
phases:
  install:
    commands:
      - echo "Activating pre-installed Erlang/OTP..."
      - source /usr/local/erlang/activate
      - export PATH=/usr/local/erlang/bin:$PATH
```

### Setup Requirements

Before deploying this hello-erlang project:

1. Deploy the Erlang image builder stack:
   ```bash
   cd /Users/jhw/work/gists/aws/8aafb8a1796b06d94ea8178beeddc9d7
   ./deploy-stack.sh erlang-image-builder
   ```

2. Build the required Erlang version image (e.g., 27.1):
   ```bash
   ./build-image.sh 27.1
   ```

3. Verify the image is available:
   ```bash
   ./list-images.sh erlang-image-builder
   ```

4. Ensure the `ErlangVersion` parameter in your deployment matches an available image tag.

### Updating Erlang Version

To use a newer Erlang version:

1. Build the new image in the erlang-codebuild project
2. Update the `ErlangVersion` parameter when deploying this stack
3. Update the stack to pick up the new image

### Related Documentation

- Erlang Image Builder: `/Users/jhw/work/gists/aws/8aafb8a1796b06d94ea8178beeddc9d7/README.md`
- Original optimization plan: Previously at `docs/codebuild-optimization.md` (now archived)
