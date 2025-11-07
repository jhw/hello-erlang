# AWS Deployment Guide

This guide covers deploying Hello Erlang to AWS EC2 using CloudFormation.

## Architecture

- **Application Load Balancer (ALB)**: HTTP/HTTPS traffic distribution
- **Target Group**: Health checks and routing to EC2:8080
- **EC2 Instance**: Runs the Erlang release
- **Security Groups**: ALB (80/443 from internet), EC2 (22 from specified IP, 8080 from ALB only)
- **Elastic IP**: Direct EC2 SSH access
- **IAM Role**: For future CloudWatch/SSM integration

## Prerequisites

1. **AWS CLI** installed and configured
   ```bash
   aws configure
   # Sets default region and credentials in ~/.aws/config
   ```

2. **Local AWS config** (optional but recommended)
   ```bash
   # Copy example config
   cp config/aws.sh.example config/aws.sh

   # Edit to customize (gitignored)
   # Set defaults for key name, subnets, instance type, SSH location, etc.
   vim config/aws.sh
   ```

   This allows you to set defaults (like `DEFAULT_KEY_NAME` and `DEFAULT_ALB_SUBNETS`) so you don't have to specify them on every command.

3. **EC2 Key Pair** created in your AWS region
   ```bash
   # Create a new key pair
   aws ec2 create-key-pair --key-name hello-erlang-dev --query 'KeyMaterial' --output text > ~/.ssh/hello-erlang-dev.pem
   chmod 400 ~/.ssh/hello-erlang-dev.pem

   # Or list existing key pairs
   aws ec2 describe-key-pairs --query 'KeyPairs[*].KeyName' --output table
   ```

4. **VPC Subnet IDs** for Application Load Balancer
   ```bash
   # Get default VPC subnet IDs (need at least 2 in different AZs)
   aws ec2 describe-subnets --filters "Name=default-for-az,Values=true" \
     --query 'Subnets[*].[SubnetId,AvailabilityZone]' --output table

   # Copy 2 or more subnet IDs for use with --subnets parameter
   ```

5. **Build tools** installed
   - Erlang/OTP
   - Rebar3

## Quick Start

### Option A: With config/aws.sh (Recommended)

```bash
# 1. Set up local config
cp config/aws.sh.example config/aws.sh

# 2. Get your subnet IDs
aws ec2 describe-subnets --filters "Name=default-for-az,Values=true" \
  --query 'Subnets[*].[SubnetId,AvailabilityZone]' --output table

# 3. Edit config/aws.sh and set:
#    export DEFAULT_KEY_NAME=hello-erlang-dev
#    export DEFAULT_ALB_SUBNETS=subnet-xxxxx,subnet-yyyyy
vim config/aws.sh

# 4. Create stack (no parameters needed - uses defaults from config)
./scripts/aws-stack.sh create dev

# This takes 3-5 minutes
# Creates: ALB, Target Group, EC2 instance, Security Groups, Elastic IP, IAM roles
```

### Option B: Without config/aws.sh (Command-line parameters)

```bash
# Get subnet IDs for ALB (copy 2 subnet IDs from output)
aws ec2 describe-subnets --filters "Name=default-for-az,Values=true" \
  --query 'Subnets[*].[SubnetId,AvailabilityZone]' --output table

# Create a development stack
./scripts/aws-stack.sh create dev \
  --key-name hello-erlang-dev \
  --subnets subnet-xxxxx,subnet-yyyyy

# This takes 3-5 minutes
# Creates: ALB, Target Group, EC2 instance, Security Groups, Elastic IP, IAM roles
```

### 2. Deploy Application

```bash
# Build and deploy in one command
./scripts/aws-deploy.sh dev

# This will:
# - Build the release tarball
# - Upload to EC2
# - Stop old release (if running)
# - Extract and start new release
# - Verify deployment
```

### 3. Access Application

```bash
# Get the ALB URL and other outputs
./scripts/aws-stack.sh outputs dev

# Test the endpoint via Load Balancer (recommended)
curl "http://YOUR_ALB_DNS_NAME/echo?message=Hello"

# Or test direct EC2 access (for debugging)
curl "http://YOUR_EC2_IP:8080/echo?message=Hello"
```

### 4. Update Code (Iterative Development)

```bash
# Make code changes
# Then redeploy:
./scripts/aws-deploy.sh dev

# Takes ~30 seconds
# Automatically stops, replaces, and restarts
```

### 5. Clean Up

```bash
# Delete the stack when done
./scripts/aws-stack.sh delete dev
```

## Commands Reference

### Stack Management

```bash
# Create stack
./scripts/aws-stack.sh create <env> --key-name <name> --subnets <subnet-ids> [--instance-type t3.small]

# Delete stack
./scripts/aws-stack.sh delete <env>

# Show stack status
./scripts/aws-stack.sh status <env>

# Show stack outputs (IPs, URLs, etc.)
./scripts/aws-stack.sh outputs <env>

# Show recent events (for debugging)
./scripts/aws-stack.sh events <env>

# List all hello-erlang stacks
./scripts/aws-stack.sh list

# Update stack (after template changes)
./scripts/aws-stack.sh update <env>
```

### Application Deployment

```bash
# Deploy application (build + upload + restart)
./scripts/aws-deploy.sh <env>

# Deploy without rebuilding (faster, uses existing tarball)
./scripts/aws-deploy.sh <env> --skip-build

# Deploy with custom SSH key
./scripts/aws-deploy.sh <env> --key-file ~/.ssh/my-key.pem
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
./scripts/aws-stack.sh create prod --key-name my-key --subnets subnet-xxx,subnet-yyy --instance-type t3.medium
```

## Workflow Examples

### Development Workflow

```bash
# One-time setup (get subnet IDs first)
SUBNETS=$(aws ec2 describe-subnets --filters "Name=default-for-az,Values=true" \
  --query 'Subnets[0:2].SubnetId' --output text | tr '\t' ',')
./scripts/aws-stack.sh create dev --key-name my-key --subnets $SUBNETS

# Iterative development
while true; do
  # Make code changes
  vim apps/hello_erlang/src/echo_handler.erl

  # Deploy (30 seconds)
  ./scripts/aws-deploy.sh dev

  # Test via ALB
  ALB_DNS=$(./scripts/aws-stack.sh outputs dev | grep LoadBalancerDNS | awk '{print $4}')
  curl "http://${ALB_DNS}/echo?message=Test"
done

# Clean up when done
./scripts/aws-stack.sh delete dev
```

### Multi-Environment Deployment

```bash
# Get subnet IDs once (use for all environments)
SUBNETS=$(aws ec2 describe-subnets --filters "Name=default-for-az,Values=true" \
  --query 'Subnets[0:2].SubnetId' --output text | tr '\t' ',')

# Deploy to dev
./scripts/aws-stack.sh create dev --key-name my-key --subnets $SUBNETS
./scripts/aws-deploy.sh dev

# Test in dev, then promote to staging
./scripts/aws-stack.sh create staging --key-name my-key --subnets $SUBNETS --instance-type t3.medium
./scripts/aws-deploy.sh staging

# Test in staging, then promote to prod
./scripts/aws-stack.sh create prod --key-name my-key --subnets $SUBNETS --instance-type t3.large
./scripts/aws-deploy.sh prod
```

## SSH Access

```bash
# Get SSH command from stack outputs
./scripts/aws-stack.sh outputs dev

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
./scripts/aws-stack.sh events dev

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

**EC2 Instances:**
- **t3.micro**: ~$0.0104/hour (~$7.50/month)
- **t3.small**: ~$0.0208/hour (~$15/month)
- **t3.medium**: ~$0.0416/hour (~$30/month)

**Application Load Balancer:**
- ALB: ~$0.0225/hour (~$16.50/month)
- LCU (Load Balancer Capacity Units): ~$0.008/LCU-hour (minimal for low traffic)

**Other:**
- Elastic IP: Free while instance is running
- Data transfer: First 1 GB free/month, then $0.09/GB

**Total for dev environment**: ~$32-35/month (t3.small + ALB)

**Tip**: Delete stacks when not in use to save costs. ALB is a fixed cost even with no traffic.

## Security Best Practices

1. **Restrict SSH access**: Use your IP instead of 0.0.0.0/0
   ```bash
   ./scripts/aws-stack.sh create dev --key-name my-key --subnets $SUBNETS --ssh-location YOUR_IP/32
   ```

2. **Application access through ALB only**: EC2 port 8080 only accepts traffic from ALB security group (not internet)

3. **Use key pairs**: Never expose private keys in version control

4. **Use IAM roles**: Already configured for CloudWatch/SSM

5. **Rotate keys**: Periodically update EC2 key pairs

6. **HTTPS/TLS**: For production, add ACM certificate and HTTPS listener to ALB (see Future Enhancements)

## Future Enhancements

### Phase 3: HTTPS/TLS
- AWS Certificate Manager (ACM) certificate
- HTTPS listener on ALB (port 443)
- HTTP to HTTPS redirect
- Custom domain configuration (Route 53)

### Phase 4: Hot Code Reloading
- Relup/appup configuration
- Zero-downtime deployments
- Version management

### Phase 5: Observability
- CloudWatch logs integration
- Custom metrics
- Alarms for errors/downtime
- Enhanced health check endpoints
- X-Ray tracing

### Phase 6: High Availability
- Auto Scaling Group (multiple EC2 instances)
- Multi-AZ deployment
- Erlang distribution/clustering
- Session persistence/sticky sessions

## Additional Resources

- [AWS CloudFormation Documentation](https://docs.aws.amazon.com/cloudformation/)
- [Erlang Release Handling](https://www.erlang.org/doc/design_principles/release_handling.html)
- [Cowboy HTTP Server](https://ninenines.eu/docs/en/cowboy/2.9/guide/)
