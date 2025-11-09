# Pipeline Alerting Strategy

## Overview

This document defines the alerting strategy for the three-stage CI/CD pipeline: **CodePipeline → CodeBuild → CodeDeploy**. The strategy uses CloudWatch Logs subscription filters (not EventBridge) for consistency, and sends all notifications to Slack via ChatOps.

**Key Decision**: CloudWatch Logs subscription filters are preferred over EventBridge for pipeline alerting because:
1. ✅ **Consistency**: All services (Pipeline, Build, Deploy) already log to CloudWatch
2. ✅ **Flexibility**: Pattern matching on log content (not just event types)
3. ✅ **Unified approach**: Single Lambda function handles all alert types
4. ✅ **Granular control**: Filter exactly what matters, ignore noise
5. ✅ **Two filters per log group**: Start/Stop events + Error events

## Alert Requirements

### What We Need to Know

| Stage | Start Event | Success Event | Failure Event | Priority |
|-------|------------|---------------|---------------|----------|
| **Pipeline** | Pipeline execution started | Pipeline succeeded | Pipeline failed | High |
| **CodeBuild** | Build started | Build succeeded | Build failed | High |
| **CodeDeploy** | Deployment started | Deployment succeeded | Deployment failed | Critical |

### Notification Examples

**Pipeline Started**:
```
🚀 Pipeline Started: dev-hello-erlang-pipeline
Environment: dev
Trigger: git push to deploy/dev
Execution ID: abc123-def456
```

**Build Failed**:
```
🔥 Build Failed: dev-hello-erlang-build
Environment: dev
Build ID: build:abc123-def456
Duration: 45 seconds
Error: Compilation failed in hello_world.erl
View logs: [CloudWatch Link]
```

**Deployment Succeeded**:
```
✅ Deployment Succeeded: dev-hello-erlang
Environment: dev
Deployment ID: d-ABCDEF123
Duration: 2m 15s
Health checks: Passing
```

## CloudWatch Log Groups

### Current Log Groups

```
# Pipeline execution logs (NEW - from EventBridge)
/aws/codepipeline/dev-hello-erlang-pipeline

# Build logs (EXISTING)
/aws/codebuild/dev-hello-erlang-build

# Deployment logs (EXISTING)
/aws/codedeploy/dev/deployments
/aws/codedeploy/dev/agent

# Application logs (EXISTING)
/aws/ec2/hello-erlang/dev
```

### Log Group Strategy

Each log group gets **two subscription filters** (AWS limit):

1. **Status Filter**: Start/Stop/Success events
2. **Error Filter**: Failure/Error events

## Subscription Filter Patterns

### CodePipeline Patterns

**Status Filter** (`/aws/codepipeline/{env}-hello-erlang-pipeline`):
```
[timestamp, request_id, level, msg="*STARTED*" || msg="*SUCCEEDED*"]
```

**Error Filter** (`/aws/codepipeline/{env}-hello-erlang-pipeline`):
```
[timestamp, request_id, level, msg="*FAILED*" || msg="*ERROR*"]
```

### CodeBuild Patterns

**Status Filter** (`/aws/codebuild/{env}-hello-erlang-build`):
```
# Match phase transitions
[msg="*Phase context status code:*" || msg="*Phase complete:*"]
```

**Error Filter** (`/aws/codebuild/{env}-hello-erlang-build`):
```
# Match build failures and errors
[msg="*BUILD FAILED*" || msg="*error:*" || msg="*ERROR:*"]
```

### CodeDeploy Patterns

**Status Filter** (`/aws/codedeploy/{env}/deployments`):
```
# Match lifecycle events
[timestamp, deployment_id, msg="*LifecycleEvent*Succeeded*"]
```

**Error Filter** (`/aws/codedeploy/{env}/deployments`):
```
# Match deployment failures
[timestamp, deployment_id, msg="*Failed*" || msg="*ERROR*"]
```

## CloudWatch Logs EventBridge Integration

To get pipeline events into CloudWatch Logs for filtering, use EventBridge:

```yaml
# EventBridge Rule: Pipeline events → CloudWatch Logs
PipelineEventsRule:
  Type: AWS::Events::Rule
  Properties:
    Name: !Sub '${Environment}-pipeline-events-to-logs'
    State: ENABLED
    EventPattern:
      source:
        - aws.codepipeline
      detail:
        pipeline:
          - !Ref CodePipeline
    Targets:
      - Arn: !GetAtt PipelineLogGroup.Arn
        Id: PipelineLogTarget

# Permission for EventBridge to write to logs
PipelineLogGroupPolicy:
  Type: AWS::Logs::ResourcePolicy
  Properties:
    PolicyName: !Sub '${Environment}-pipeline-events-policy'
    PolicyDocument: !Sub |
      {
        "Version": "2012-10-17",
        "Statement": [{
          "Effect": "Allow",
          "Principal": {
            "Service": "events.amazonaws.com"
          },
          "Action": [
            "logs:CreateLogStream",
            "logs:PutLogEvents"
          ],
          "Resource": "${PipelineLogGroup.Arn}"
        }]
      }
```

## Lambda Alert Function

### Single Function for All Alerts

One Lambda function handles alerts from all subscription filters:

```python
import json
import urllib3
import os
import base64
import gzip
import re
from datetime import datetime

http = urllib3.PoolManager()

SLACK_WEBHOOK_URL = os.environ['SLACK_WEBHOOK_URL']
REGION = os.environ['AWS_REGION']

def handler(event, context):
    """
    Unified alert handler for CodePipeline, CodeBuild, and CodeDeploy
    Triggered by CloudWatch Logs subscription filters
    """

    # Decode CloudWatch Logs data
    log_data = json.loads(
        gzip.decompress(
            base64.b64decode(event['awslogs']['data'])
        ).decode('utf-8')
    )

    log_group = log_data['logGroup']
    log_stream = log_data['logStream']

    # Determine service from log group
    if '/codepipeline/' in log_group:
        service_type = 'pipeline'
    elif '/codebuild/' in log_group:
        service_type = 'build'
    elif '/codedeploy/' in log_group:
        service_type = 'deploy'
    else:
        service_type = 'unknown'

    # Process each log event
    for log_event in log_data['logEvents']:
        message = log_event['message']
        timestamp = log_event['timestamp']

        # Parse and send appropriate notification
        alert = parse_alert(service_type, message, log_group, log_stream, timestamp)
        if alert:
            send_slack_notification(alert)

    return {'statusCode': 200}


def parse_alert(service_type, message, log_group, log_stream, timestamp):
    """Parse log message and create structured alert"""

    # Extract environment from log group
    env_match = re.search(r'/(dev|staging|prod)', log_group)
    environment = env_match.group(1) if env_match else 'unknown'

    alert = {
        'service': service_type,
        'environment': environment,
        'log_group': log_group,
        'log_stream': log_stream,
        'timestamp': timestamp,
        'raw_message': message
    }

    if service_type == 'pipeline':
        return parse_pipeline_alert(message, alert)
    elif service_type == 'build':
        return parse_build_alert(message, alert)
    elif service_type == 'deploy':
        return parse_deploy_alert(message, alert)

    return alert


def parse_pipeline_alert(message, alert):
    """Parse CodePipeline events"""
    msg_lower = message.lower()

    if 'started' in msg_lower:
        alert['event_type'] = 'started'
        alert['icon'] = '🚀'
        alert['color'] = '#0066cc'
        alert['title'] = 'Pipeline Started'
    elif 'succeeded' in msg_lower:
        alert['event_type'] = 'succeeded'
        alert['icon'] = '✅'
        alert['color'] = 'good'
        alert['title'] = 'Pipeline Succeeded'
    elif 'failed' in msg_lower:
        alert['event_type'] = 'failed'
        alert['icon'] = '🔥'
        alert['color'] = 'danger'
        alert['title'] = 'Pipeline Failed'
    else:
        return None

    # Extract execution ID if present
    exec_match = re.search(r'execution[/-]id[:\s]+([a-zA-Z0-9-]+)', message)
    if exec_match:
        alert['execution_id'] = exec_match.group(1)

    return alert


def parse_build_alert(message, alert):
    """Parse CodeBuild events"""
    msg_lower = message.lower()

    # Detect build phase
    if 'phase complete:' in msg_lower or 'phase context status' in msg_lower:
        if 'install' in msg_lower:
            phase = 'install'
        elif 'pre_build' in msg_lower:
            phase = 'pre_build'
        elif 'build' in msg_lower:
            phase = 'build'
        elif 'post_build' in msg_lower:
            phase = 'post_build'
        else:
            return None  # Ignore other phases

        alert['event_type'] = 'phase_complete'
        alert['phase'] = phase
        alert['icon'] = '⚙️'
        alert['color'] = '#808080'
        alert['title'] = f'Build Phase: {phase}'
        return alert

    # Detect build status
    if 'build failed' in msg_lower or 'error:' in msg_lower:
        alert['event_type'] = 'failed'
        alert['icon'] = '🔥'
        alert['color'] = 'danger'
        alert['title'] = 'Build Failed'

        # Extract error details
        error_match = re.search(r'error:\s*(.+)', message, re.IGNORECASE)
        if error_match:
            alert['error'] = error_match.group(1)[:200]

    elif 'build succeeded' in msg_lower or 'artifacts uploaded' in msg_lower:
        alert['event_type'] = 'succeeded'
        alert['icon'] = '✅'
        alert['color'] = 'good'
        alert['title'] = 'Build Succeeded'

    else:
        return None

    # Extract build ID
    build_match = re.search(r'build[/-]id[:\s]+([a-zA-Z0-9:-]+)', message)
    if build_match:
        alert['build_id'] = build_match.group(1)

    return alert


def parse_deploy_alert(message, alert):
    """Parse CodeDeploy events"""
    msg_lower = message.lower()

    # Lifecycle events
    if 'lifecycleevent' in msg_lower:
        if 'succeeded' in msg_lower:
            # Determine which lifecycle hook
            if 'applicationstart' in msg_lower:
                hook = 'ApplicationStart'
            elif 'validateservice' in msg_lower:
                hook = 'ValidateService'
            elif 'afterinstall' in msg_lower:
                hook = 'AfterInstall'
            elif 'beforeinstall' in msg_lower:
                hook = 'BeforeInstall'
            elif 'applicationstop' in msg_lower:
                hook = 'ApplicationStop'
            else:
                return None

            alert['event_type'] = 'lifecycle_succeeded'
            alert['hook'] = hook
            alert['icon'] = '✅' if hook == 'ValidateService' else '⚙️'
            alert['color'] = 'good' if hook == 'ValidateService' else '#808080'
            alert['title'] = f'Deploy: {hook}'
        elif 'failed' in msg_lower:
            alert['event_type'] = 'lifecycle_failed'
            alert['icon'] = '🔥'
            alert['color'] = 'danger'
            alert['title'] = 'Deployment Failed'

    elif 'deployment' in msg_lower:
        if 'succeeded' in msg_lower:
            alert['event_type'] = 'succeeded'
            alert['icon'] = '✅'
            alert['color'] = 'good'
            alert['title'] = 'Deployment Succeeded'
        elif 'failed' in msg_lower or 'error' in msg_lower:
            alert['event_type'] = 'failed'
            alert['icon'] = '🔥'
            alert['color'] = 'danger'
            alert['title'] = 'Deployment Failed'
    else:
        return None

    # Extract deployment ID
    deploy_match = re.search(r'deployment[/-]id[:\s]+([a-zA-Z0-9-]+)', message)
    if deploy_match:
        alert['deployment_id'] = deploy_match.group(1)

    return alert


def send_slack_notification(alert):
    """Send formatted notification to Slack"""

    # Build CloudWatch Logs Insights URL
    log_url = build_log_url(
        alert['log_group'],
        alert['log_stream'],
        alert['timestamp']
    )

    # Build fields for Slack attachment
    fields = [
        {
            'title': 'Environment',
            'value': alert['environment'].upper(),
            'short': True
        },
        {
            'title': 'Service',
            'value': alert['service'].capitalize(),
            'short': True
        }
    ]

    # Add service-specific fields
    if 'execution_id' in alert:
        fields.append({
            'title': 'Execution ID',
            'value': alert['execution_id'],
            'short': True
        })

    if 'build_id' in alert:
        fields.append({
            'title': 'Build ID',
            'value': alert['build_id'],
            'short': False
        })

    if 'deployment_id' in alert:
        fields.append({
            'title': 'Deployment ID',
            'value': alert['deployment_id'],
            'short': True
        })

    if 'phase' in alert:
        fields.append({
            'title': 'Phase',
            'value': alert['phase'],
            'short': True
        })

    if 'hook' in alert:
        fields.append({
            'title': 'Lifecycle Hook',
            'value': alert['hook'],
            'short': True
        })

    if 'error' in alert:
        fields.append({
            'title': 'Error',
            'value': f"```{alert['error']}```",
            'short': False
        })

    # Build Slack message
    slack_message = {
        'text': f"{alert['icon']} {alert['title']}",
        'attachments': [{
            'color': alert['color'],
            'fields': fields,
            'footer': alert['log_group'],
            'footer_icon': 'https://a0.awsstatic.com/libra-css/images/logos/aws_logo_smile_1200x630.png',
            'ts': alert['timestamp'] // 1000,
            'actions': [{
                'type': 'button',
                'text': 'View Logs',
                'url': log_url
            }]
        }]
    }

    # Send to Slack
    try:
        response = http.request(
            'POST',
            SLACK_WEBHOOK_URL,
            body=json.dumps(slack_message),
            headers={'Content-Type': 'application/json'}
        )
        print(f"Slack notification sent: {response.status}")
    except Exception as e:
        print(f"Error sending Slack notification: {e}")


def build_log_url(log_group, log_stream, timestamp):
    """Build CloudWatch Logs Insights URL"""

    # Calculate time range (±5 minutes around event)
    start_time = (timestamp - 300000) // 1000  # 5 min before
    end_time = (timestamp + 300000) // 1000    # 5 min after

    # Encode log group name
    import urllib.parse
    log_group_encoded = urllib.parse.quote(log_group)

    # Build URL
    url = (
        f"https://{REGION}.console.aws.amazon.com/cloudwatch/home"
        f"?region={REGION}"
        f"#logsV2:log-groups/log-group/{log_group_encoded}"
        f"/log-events/{urllib.parse.quote(log_stream)}"
        f"$3FfilterPattern$3D$26start$3D{start_time}$26end$3D{end_time}"
    )

    return url
```

## CloudFormation Resources

### Lambda Function and Permissions

```yaml
Resources:
  # Unified alert Lambda function
  PipelineAlertFunction:
    Type: AWS::Lambda::Function
    Properties:
      FunctionName: !Sub '${Environment}-hello-erlang-pipeline-alerts'
      Runtime: python3.11
      Handler: index.handler
      Timeout: 30
      MemorySize: 256
      Role: !GetAtt PipelineAlertRole.Arn
      Environment:
        Variables:
          SLACK_WEBHOOK_URL: !Ref SlackWebhookUrl
          ENVIRONMENT: !Ref Environment
      Code:
        ZipFile: |
          # (Lambda code from above)

  # IAM Role for Lambda
  PipelineAlertRole:
    Type: AWS::IAM::Role
    Properties:
      RoleName: !Sub '${Environment}-pipeline-alert-role'
      AssumeRolePolicyDocument:
        Version: '2012-10-17'
        Statement:
          - Effect: Allow
            Principal:
              Service: lambda.amazonaws.com
            Action: sts:AssumeRole
      ManagedPolicyArns:
        - arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole

  # CloudWatch Log Groups
  PipelineLogGroup:
    Type: AWS::Logs::LogGroup
    Properties:
      LogGroupName: !Sub '/aws/codepipeline/${Environment}-hello-erlang-pipeline'
      RetentionInDays: 14

  # Subscription Filters (2 per log group)

  # Pipeline: Status filter
  PipelineStatusFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Ref PipelineLogGroup
      FilterPattern: '[timestamp, request_id, level, msg="*STARTED*" || msg="*SUCCEEDED*"]'
      DestinationArn: !GetAtt PipelineAlertFunction.Arn

  # Pipeline: Error filter
  PipelineErrorFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Ref PipelineLogGroup
      FilterPattern: '[timestamp, request_id, level, msg="*FAILED*" || msg="*ERROR*"]'
      DestinationArn: !GetAtt PipelineAlertFunction.Arn

  # CodeBuild: Status filter
  BuildStatusFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Sub '/aws/codebuild/${Environment}-hello-erlang-build'
      FilterPattern: '[msg="*Phase complete:*" || msg="*Artifacts uploaded*"]'
      DestinationArn: !GetAtt PipelineAlertFunction.Arn

  # CodeBuild: Error filter
  BuildErrorFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Sub '/aws/codebuild/${Environment}-hello-erlang-build'
      FilterPattern: '[msg="*BUILD FAILED*" || msg="*error:*" || msg="*ERROR:*"]'
      DestinationArn: !GetAtt PipelineAlertFunction.Arn

  # CodeDeploy: Status filter
  DeployStatusFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Sub '/aws/codedeploy/${Environment}/deployments'
      FilterPattern: '[timestamp, deployment_id, msg="*LifecycleEvent*Succeeded*"]'
      DestinationArn: !GetAtt PipelineAlertFunction.Arn

  # CodeDeploy: Error filter
  DeployErrorFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Sub '/aws/codedeploy/${Environment}/deployments'
      FilterPattern: '[timestamp, deployment_id, msg="*Failed*" || msg="*ERROR*"]'
      DestinationArn: !GetAtt PipelineAlertFunction.Arn

  # Lambda permissions (one per log group)
  PipelineLogsPermission:
    Type: AWS::Lambda::Permission
    Properties:
      FunctionName: !Ref PipelineAlertFunction
      Action: lambda:InvokeFunction
      Principal: logs.amazonaws.com
      SourceArn: !GetAtt PipelineLogGroup.Arn

  BuildLogsPermission:
    Type: AWS::Lambda::Permission
    Properties:
      FunctionName: !Ref PipelineAlertFunction
      Action: lambda:InvokeFunction
      Principal: logs.amazonaws.com
      SourceArn: !Sub 'arn:aws:logs:${AWS::Region}:${AWS::AccountId}:log-group:/aws/codebuild/${Environment}-hello-erlang-build:*'

  DeployLogsPermission:
    Type: AWS::Lambda::Permission
    Properties:
      FunctionName: !Ref PipelineAlertFunction
      Action: lambda:InvokeFunction
      Principal: logs.amazonaws.com
      SourceArn: !Sub 'arn:aws:logs:${AWS::Region}:${AWS::AccountId}:log-group:/aws/codedeploy/${Environment}/deployments:*'

Parameters:
  SlackWebhookUrl:
    Type: String
    NoEcho: true
    Description: Slack webhook URL for pipeline alerts
```

## Alert Flow Diagram

```
┌─────────────────────┐
│  CodePipeline       │
│  (Source → Build →  │
│   Deploy stages)    │
└──────────┬──────────┘
           │
           ├─ Pipeline events → EventBridge → /aws/codepipeline/{env}/logs
           │                                         │
           │                                         ├─ Status Filter (started, succeeded)
           │                                         └─ Error Filter (failed, error)
           │
           ├─ CodeBuild logs → /aws/codebuild/{env}-build
           │                         │
           │                         ├─ Status Filter (phase complete, artifacts)
           │                         └─ Error Filter (build failed, errors)
           │
           └─ CodeDeploy logs → /aws/codedeploy/{env}/deployments
                                      │
                                      ├─ Status Filter (lifecycle succeeded)
                                      └─ Error Filter (failed, errors)
                                               │
                                               ↓
                                    ┌──────────────────────┐
                                    │  Lambda Function     │
                                    │  (Parse & Format)    │
                                    └──────────┬───────────┘
                                               │
                                               ↓
                                    ┌──────────────────────┐
                                    │  Slack Webhook       │
                                    │  #deployments        │
                                    └──────────────────────┘
```

## Slack Channel Setup

### Recommended Channel Structure

```
#deployments-dev      → Dev environment alerts
#deployments-staging  → Staging environment alerts
#deployments-prod     → Prod environment alerts (critical only)
```

### Webhook Configuration

```bash
# Store webhook URLs in Secrets Manager (per environment)
aws secretsmanager create-secret \
    --name dev/slack-webhook-deployments \
    --description "Slack webhook for dev deployment alerts" \
    --secret-string "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"

# Reference in CloudFormation
SlackWebhookUrl:
  Type: AWS::SecretsManager::Secret
  Properties:
    Name: !Sub '${Environment}/slack-webhook-deployments'
    SecretString: !Ref SlackWebhookUrlParameter
```

## Alert Filtering Strategy

### What to Alert On

| Alert Type | Dev | Staging | Prod |
|------------|-----|---------|------|
| Pipeline Started | ✅ | ✅ | ✅ |
| Pipeline Succeeded | ❌ | ✅ | ✅ |
| Pipeline Failed | ✅ | ✅ | ✅ |
| Build Started | ❌ | ❌ | ❌ |
| Build Succeeded | ❌ | ✅ | ✅ |
| Build Failed | ✅ | ✅ | ✅ |
| Deploy Started | ❌ | ❌ | ✅ |
| Deploy Succeeded | ✅ | ✅ | ✅ |
| Deploy Failed | ✅ | ✅ | ✅ |

### Implementation via Lambda Logic

```python
def should_alert(alert, environment):
    """Determine if alert should be sent based on environment"""

    event_type = alert['event_type']
    service = alert['service']

    # Production: alert on everything
    if environment == 'prod':
        return True

    # Staging: skip build start, pipeline success
    if environment == 'staging':
        if event_type == 'started' and service == 'build':
            return False
        if event_type == 'succeeded' and service == 'pipeline':
            return False
        return True

    # Dev: only alert on failures and final deploy
    if environment == 'dev':
        if event_type in ['failed', 'error']:
            return True
        if event_type == 'succeeded' and service == 'deploy':
            return True
        return False

    return True
```

## Testing Alerts

### Test Pipeline

```bash
# Trigger pipeline
git push origin main:deploy/dev

# Watch for Slack notifications:
# 1. 🚀 Pipeline Started
# 2. ⚙️ Build phases (if enabled)
# 3. ⚙️ Deploy lifecycle hooks
# 4. ✅ Deployment Succeeded
```

### Test Build Failure

```erlang
% Introduce syntax error in apps/hello_erlang/src/echo_handler.erl
init(Req0, State) ->
    intentional_syntax_error  % Missing comma, no match

% Push change
git add .
git commit -m "Test build failure alert"
git push origin main:deploy/dev

% Expect Slack notification:
% 🔥 Build Failed
% Error: syntax error before: 'intentional_syntax_error'
```

### Test Deployment Failure

```bash
# Break a deployment script
echo "exit 1" >> config/aws/codedeploy/validate_service.sh
git add .
git commit -m "Test deployment failure alert"
git push origin main:deploy/dev

# Expect Slack notification:
# 🔥 Deployment Failed
# Hook: ValidateService
# Error: Script exited with code 1
```

## Comparison: EventBridge vs CloudWatch Logs

### Why CloudWatch Logs is Better

| Feature | EventBridge | CloudWatch Logs | Winner |
|---------|-------------|-----------------|--------|
| **Consistency** | Separate config per service | Unified subscription filters | Logs ✅ |
| **Granularity** | Event type level only | Message content matching | Logs ✅ |
| **Flexibility** | Fixed event schemas | Regex pattern matching | Logs ✅ |
| **Debugging** | View events separately | All logs in one place | Logs ✅ |
| **Cost** | Free (under limits) | $0.50/GB ingested | EventBridge ✅ |
| **Latency** | Near real-time | 1-2 sec delay | EventBridge ✅ |

### EventBridge Limitations for Our Use Case

1. **CodeBuild events are too noisy**: Every phase transition triggers event
2. **CodeDeploy events lack context**: Don't include log messages
3. **Pipeline events are verbose**: Includes stage/action details we don't need
4. **Multiple event patterns required**: One rule per alert type
5. **Can't filter on log content**: Only on event metadata

### CloudWatch Logs Advantages

1. ✅ **Unified approach**: All services already log to CloudWatch
2. ✅ **Content-based filtering**: Match on error messages, not just event types
3. ✅ **Two filters per log group**: Status + Errors
4. ✅ **Single Lambda function**: Handles all alert types
5. ✅ **Debugging friendly**: Alerts link back to log context

## Monitoring Alert System Health

### Lambda Function Metrics

```bash
# Check Lambda invocations
aws cloudwatch get-metric-statistics \
    --namespace AWS/Lambda \
    --metric-name Invocations \
    --dimensions Name=FunctionName,Value=dev-hello-erlang-pipeline-alerts \
    --start-time 2024-01-01T00:00:00Z \
    --end-time 2024-01-02T00:00:00Z \
    --period 3600 \
    --statistics Sum

# Check Lambda errors
aws cloudwatch get-metric-statistics \
    --namespace AWS/Lambda \
    --metric-name Errors \
    --dimensions Name=FunctionName,Value=dev-hello-erlang-pipeline-alerts \
    --start-time 2024-01-01T00:00:00Z \
    --end-time 2024-01-02T00:00:00Z \
    --period 3600 \
    --statistics Sum
```

### Subscription Filter Metrics

```bash
# Check subscription filter delivery
aws logs describe-subscription-filters \
    --log-group-name /aws/codepipeline/dev-hello-erlang-pipeline
```

### Dead Letter Queue (Optional)

```yaml
# Add DLQ for failed Lambda invocations
PipelineAlertDLQ:
  Type: AWS::SQS::Queue
  Properties:
    QueueName: !Sub '${Environment}-pipeline-alerts-dlq'
    MessageRetentionPeriod: 1209600  # 14 days

PipelineAlertFunction:
  Properties:
    DeadLetterConfig:
      TargetArn: !GetAtt PipelineAlertDLQ.Arn
```

## Implementation Checklist

- [ ] Create Lambda function with unified alert handler
- [ ] Add Slack webhook URL to Secrets Manager (per environment)
- [ ] Create CloudWatch Log Groups for pipeline events
- [ ] Set up EventBridge rule: Pipeline events → CloudWatch Logs
- [ ] Create subscription filters for CodePipeline (status + error)
- [ ] Create subscription filters for CodeBuild (status + error)
- [ ] Create subscription filters for CodeDeploy (status + error)
- [ ] Add Lambda permissions for each log group
- [ ] Configure Slack channels (#deployments-dev, etc.)
- [ ] Test with manual pipeline trigger
- [ ] Test with intentional build failure
- [ ] Test with intentional deployment failure
- [ ] Document alert types in team wiki
- [ ] Set up on-call rotation for prod alerts

## Future Enhancements

### 1. Alert Aggregation
Batch multiple alerts within 5-minute window to reduce Slack noise:

```python
# Use DynamoDB to track recent alerts
# Send digest every 5 minutes instead of immediate alerts
```

### 2. Alert Acknowledgment
Add Slack buttons to acknowledge alerts:

```json
{
  "actions": [
    {
      "type": "button",
      "text": "Acknowledge",
      "value": "ack",
      "style": "primary"
    },
    {
      "type": "button",
      "text": "View Logs",
      "url": "https://..."
    }
  ]
}
```

### 3. PagerDuty Integration
For production critical alerts:

```python
if environment == 'prod' and event_type == 'failed':
    trigger_pagerduty_incident(alert)
```

### 4. Metrics Dashboard
Create CloudWatch Dashboard showing:
- Pipeline execution success rate
- Average build time
- Deployment frequency
- Time to deploy (commit → production)

## Related Documents
- [CodePipeline Migration Plan](./codepipeline-migration.md)
- [Logging Strategy](./logging-strategy.md) - Application-level logging
- [CodeBuild Optimization](./codebuild-optimization.md)
