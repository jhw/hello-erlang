# AWS Deployment Guide

This guide covers deploying Hello Erlang to AWS EC2 using CloudFormation.

## Architecture

- **EC2 Instance**: Runs the Erlang release
- **Security Group**: SSH (22) and HTTP (8080) access
- **Elastic IP**: Stable public IP address
- **IAM Role**: For future CloudWatch/SSM integration

## Prerequisites

1. **AWS CLI** installed and configured
   ```bash
   aws configure
   ```

2. **Project AWS Configuration** (Optional but recommended)
   ```bash
   # Copy example config
   cp config/aws-local.sh.example config/aws-local.sh

   # Edit to set your preferences
   vim config/aws-local.sh

   # Example:
   # export AWS_REGION=us-west-2
   # export AWS_PROFILE=myprofile
   # export DEFAULT_SSH_LOCATION=YOUR_IP/32
   ```

   **Configuration Hierarchy:**
   1. `config/aws-local.sh` (gitignored, your personal settings)
   2. `config/aws-defaults.sh` (committed, project defaults)
   3. `~/.aws/config` (AWS CLI defaults)
   4. Environment variables (highest priority)

3. **EC2 Key Pair** created in your AWS region
   ```bash
   # Create a new key pair
   aws ec2 create-key-pair --key-name hello-erlang-dev --query 'KeyMaterial' --output text > ~/.ssh/hello-erlang-dev.pem
   chmod 400 ~/.ssh/hello-erlang-dev.pem

   # Or list existing key pairs
   aws ec2 describe-key-pairs --query 'KeyPairs[*].KeyName' --output table
   ```

4. **Build tools** installed
   - Erlang/OTP
   - Rebar3

## Quick Start

### 1. Create Infrastructure

```bash
# Create a development stack
./scripts/aws/stack.sh create dev --key-name hello-erlang-dev

# This takes 3-5 minutes
# Creates: EC2 instance, Security Group, Elastic IP, IAM roles
```

### 2. Deploy Application

```bash
# Build and deploy in one command
./scripts/aws/deploy.sh dev

# This will:
# - Build the release tarball
# - Upload to EC2
# - Stop old release (if running)
# - Extract and start new release
# - Verify deployment
```

### 3. Access Application

```bash
# Get the public IP and URL
./scripts/aws/stack.sh outputs dev

# Test the endpoint
curl "http://YOUR_IP:8080/echo?message=Hello"
```

### 4. Update Code (Iterative Development)

```bash
# Make code changes
# Then redeploy:
./scripts/aws/deploy.sh dev

# Takes ~30 seconds
# Automatically stops, replaces, and restarts
```

### 5. Clean Up

```bash
# Delete the stack when done
./scripts/aws/stack.sh delete dev
```

## Commands Reference

### Stack Management

```bash
# Create stack
./scripts/aws/stack.sh create <env> --key-name <name> [--instance-type t3.small]

# Delete stack
./scripts/aws/stack.sh delete <env>

# Show stack status
./scripts/aws/stack.sh status <env>

# Show stack outputs (IPs, URLs, etc.)
./scripts/aws/stack.sh outputs <env>

# Show recent events (for debugging)
./scripts/aws/stack.sh events <env>

# List all hello-erlang stacks
./scripts/aws/stack.sh list

# Update stack (after template changes)
./scripts/aws/stack.sh update <env>
```

### Application Deployment

```bash
# Deploy application (build + upload + restart)
./scripts/aws/deploy.sh <env>

# Deploy without rebuilding (faster, uses existing tarball)
./scripts/aws/deploy.sh <env> --skip-build

# Deploy with custom SSH key
./scripts/aws/deploy.sh <env> --key-file ~/.ssh/my-key.pem
```

## Environments

Three environments are supported: `dev`, `staging`, `prod`

Each environment gets its own isolated stack:
- `hello-erlang-dev`
- `hello-erlang-staging`
- `hello-erlang-prod`

## Instance Types

Default: `t3.small` (2 vCPU, 2 GB RAM)

Available options:
- `t3.micro` - 2 vCPU, 1 GB RAM (cheapest, limited)
- `t3.small` - 2 vCPU, 2 GB RAM (recommended for dev)
- `t3.medium` - 2 vCPU, 4 GB RAM (recommended for staging/prod)
- `t3.large` - 2 vCPU, 8 GB RAM (high traffic)

Specify during stack creation:
```bash
./scripts/aws/stack.sh create prod --key-name my-key --instance-type t3.medium
```

## Workflow Examples

### Development Workflow

```bash
# One-time setup
./scripts/aws/stack.sh create dev --key-name my-key

# Iterative development
while true; do
  # Make code changes
  vim apps/hello_erlang/src/echo_handler.erl

  # Deploy (30 seconds)
  ./scripts/aws/deploy.sh dev

  # Test
  curl "http://$(./scripts/aws/stack.sh outputs dev | grep PublicIP | awk '{print $4}'):8080/echo?message=Test"
done

# Clean up when done
./scripts/aws/stack.sh delete dev
```

### Multi-Environment Deployment

```bash
# Deploy to dev
./scripts/aws/stack.sh create dev --key-name my-key
./scripts/aws/deploy.sh dev

# Test in dev, then promote to staging
./scripts/aws/stack.sh create staging --key-name my-key --instance-type t3.medium
./scripts/aws/deploy.sh staging

# Test in staging, then promote to prod
./scripts/aws/stack.sh create prod --key-name my-key --instance-type t3.large
./scripts/aws/deploy.sh prod
```

## SSH Access

```bash
# Get SSH command from stack outputs
./scripts/aws/stack.sh outputs dev

# Or construct manually
ssh -i ~/.ssh/hello-erlang-dev.pem ec2-user@YOUR_IP

# Once connected, you can:
cd /opt/hello_erlang

# Check status
./bin/hello_erlang pid

# Attach to console
./bin/hello_erlang remote_console

# View logs
ls -la log/
```

## Troubleshooting

### Stack Creation Fails

```bash
# Check events for error details
./scripts/aws/stack.sh events dev

# Common issues:
# - Key pair doesn't exist in the region
# - Instance type not available in region
# - Security group limits reached
```

### Deployment Fails

```bash
# SSH in and check manually
ssh -i ~/.ssh/your-key.pem ec2-user@YOUR_IP
cd /opt/hello_erlang

# Check if release exists
ls -la

# Try starting manually
./bin/hello_erlang daemon
./bin/hello_erlang pid

# Check logs
cat log/erlang.log.*
```

### Application Not Responding

```bash
# Check if process is running
ssh -i ~/.ssh/your-key.pem ec2-user@YOUR_IP
cd /opt/hello_erlang
./bin/hello_erlang pid

# Check security group allows port 8080
# Check application logs
```

## Cost Estimation

Approximate AWS costs (us-east-1, on-demand):

- **t3.micro**: ~$0.0104/hour (~$7.50/month)
- **t3.small**: ~$0.0208/hour (~$15/month)
- **t3.medium**: ~$0.0416/hour (~$30/month)

Plus:
- Elastic IP: Free while instance is running
- Data transfer: First 1 GB free/month

**Total for dev environment**: ~$15-20/month

**Tip**: Delete stacks when not in use to save costs.

## Security Best Practices

1. **Restrict SSH access**: Use your IP instead of 0.0.0.0/0
   ```bash
   ./scripts/aws/stack.sh create dev --key-name my-key --ssh-location YOUR_IP/32
   ```

2. **Use key pairs**: Never expose private keys in version control

3. **Use IAM roles**: Already configured for CloudWatch/SSM

4. **Rotate keys**: Periodically update EC2 key pairs

## Future Enhancements

### Phase 3: Hot Code Reloading
- Relup/appup configuration
- Zero-downtime deployments
- Version management

### Phase 4: Observability
- CloudWatch logs integration
- Custom metrics
- Alarms for errors/downtime
- Health check endpoints

### Phase 5: High Availability
- Application Load Balancer
- Auto Scaling Group
- Multi-AZ deployment
- Erlang distribution/clustering

## Additional Resources

- [AWS CloudFormation Documentation](https://docs.aws.amazon.com/cloudformation/)
- [Erlang Release Handling](https://www.erlang.org/doc/design_principles/release_handling.html)
- [Cowboy HTTP Server](https://ninenines.eu/docs/en/cowboy/2.9/guide/)
