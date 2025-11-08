# Logging Strategy

## Overview

This document outlines the non-invasive logging strategy for the hello-erlang application, mirroring the Lambda pattern of automatic error capture and notification.

**Key Principle:** Zero changes to handler code. All logging configured through `sys.config` and infrastructure.

## Pattern Summary

| Environment | Error Source | Transport | Destination | Invasiveness |
|-------------|-------------|-----------|-------------|--------------|
| **AWS EC2** | Erlang Logger → File | CloudWatch Agent | CloudWatch Logs → Subscription Filter → Lambda → Slack | **Zero code changes** |
| **Localhost** | Erlang Logger → File | File watcher script | Desktop notification / Slack | **Zero code changes** |

## 1. Erlang Error Handling

### Built-in Error Reporting

Erlang's `logger` module (OTP 21+) automatically captures:
- Process crashes
- Supervisor restart reports
- Application failures
- Uncaught exceptions

### Configuration (sys.config)

Configure logging entirely through `config/sys.config`:

```erlang
[
  {kernel, [
    {logger_level, info},
    {logger, [
      %% Console handler (for local dev)
      {handler, default, logger_std_h, #{
        level => info,
        formatter => {logger_formatter, #{
          single_line => false,
          template => [time, " [", level, "] ", msg, "\n"]
        }}
      }},

      %% File handler (for CloudWatch Agent)
      {handler, file_handler, logger_std_h, #{
        level => error,
        config => #{
          file => "/var/log/hello_erlang/errors.log",
          max_no_bytes => 10485760,  % 10MB
          max_no_files => 5,
          filesync_repeat_interval => 5000
        },
        formatter => {logger_formatter, #{
          single_line => true,  % Better for CloudWatch parsing
          template => [time, " ", level, " ", msg, "\n"]
        }}
      }},

      %% Disk log handler for crashes
      {handler, crash_handler, logger_disk_log_h, #{
        level => error,
        config => #{
          file => "/var/log/hello_erlang/crashes",
          type => halt,
          max_no_bytes => 10485760,
          max_no_files => 5
        }
      }}
    ]}
  ]},

  %% SASL - System Application Support Libraries
  %% Captures supervisor reports, crash reports automatically
  {sasl, [
    {sasl_error_logger, false},  % Let logger handle it
    {errlog_type, error}
  ]},

  {hello_erlang, []}
].
```

**Benefits:**
- All crashes/errors automatically logged to `/var/log/hello_erlang/errors.log`
- Log rotation handled by Erlang
- Structured formatting for CloudWatch parsing
- No application code changes required

## 2. EC2 CloudWatch Integration

### CloudWatch Agent

The CloudWatch Agent watches log files and streams them to CloudWatch Logs.

**IAM Role:** Already configured in CloudFormation with `CloudWatchAgentServerPolicy`

### UserData Installation

Add to `config/aws/stack.yaml` EC2Instance UserData:

```yaml
UserData:
  Fn::Base64: !Sub |
    #!/bin/bash
    set -e

    # System updates
    yum update -y
    yum install -y wget tar gzip

    # Install CloudWatch Agent
    wget https://s3.amazonaws.com/amazoncloudwatch-agent/amazon_linux/amd64/latest/amazon-cloudwatch-agent.rpm
    rpm -U ./amazon-cloudwatch-agent.rpm

    # Create log directory
    mkdir -p /var/log/hello_erlang
    chown ec2-user:ec2-user /var/log/hello_erlang

    # Create CloudWatch Agent config
    cat > /opt/aws/amazon-cloudwatch-agent/etc/config.json <<'EOF'
    {
      "logs": {
        "logs_collected": {
          "files": {
            "collect_list": [
              {
                "file_path": "/var/log/hello_erlang/errors.log",
                "log_group_name": "/aws/ec2/hello-erlang/${Environment}",
                "log_stream_name": "{instance_id}/errors",
                "timezone": "UTC",
                "retention_in_days": 7
              },
              {
                "file_path": "/opt/hello_erlang/log/erlang.log.*",
                "log_group_name": "/aws/ec2/hello-erlang/${Environment}",
                "log_stream_name": "{instance_id}/console",
                "timezone": "UTC",
                "retention_in_days": 3
              }
            ]
          }
        }
      },
      "metrics": {
        "namespace": "HelloErlang/${Environment}",
        "metrics_collected": {
          "mem": {
            "measurement": [
              {"name": "mem_used_percent", "rename": "MemoryUsed", "unit": "Percent"}
            ],
            "metrics_collection_interval": 60
          },
          "disk": {
            "measurement": [
              {"name": "used_percent", "rename": "DiskUsed", "unit": "Percent"}
            ],
            "metrics_collection_interval": 60,
            "resources": ["*"]
          }
        }
      }
    }
    EOF

    # Start CloudWatch Agent
    /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \
      -a fetch-config \
      -m ec2 \
      -s \
      -c file:/opt/aws/amazon-cloudwatch-agent/etc/config.json

    # Application deployment directory
    mkdir -p /opt/hello_erlang
    chown ec2-user:ec2-user /opt/hello_erlang
```

## 3. CloudWatch Subscription Filters to Slack

### Lambda Error Notifier

Add to `config/aws/stack.yaml`:

```yaml
Resources:
  # Log Group
  ErrorLogGroup:
    Type: AWS::Logs::LogGroup
    Properties:
      LogGroupName: !Sub /aws/ec2/hello-erlang/${Environment}
      RetentionInDays: 7

  # Subscription Filter (filters for errors)
  ErrorSubscriptionFilter:
    Type: AWS::Logs::SubscriptionFilter
    Properties:
      LogGroupName: !Ref ErrorLogGroup
      FilterPattern: '[time, level="error" || level="critical", ...]'
      DestinationArn: !GetAtt ErrorNotifierFunction.Arn

  # Lambda Function (sends to Slack)
  ErrorNotifierFunction:
    Type: AWS::Lambda::Function
    Properties:
      FunctionName: !Sub ${Environment}-hello-erlang-error-notifier
      Runtime: python3.11
      Handler: index.handler
      Role: !GetAtt ErrorNotifierRole.Arn
      Environment:
        Variables:
          SLACK_WEBHOOK_URL: !Ref SlackWebhookUrl
          ENVIRONMENT: !Ref Environment
      Code:
        ZipFile: |
          import json
          import urllib3
          import os
          import base64
          import gzip

          http = urllib3.PoolManager()

          def handler(event, context):
              webhook_url = os.environ['SLACK_WEBHOOK_URL']
              environment = os.environ.get('ENVIRONMENT', 'unknown')

              # Decode CloudWatch Logs data
              log_data = json.loads(
                  gzip.decompress(
                      base64.b64decode(event['awslogs']['data'])
                  ).decode('utf-8')
              )

              for log_event in log_data['logEvents']:
                  message = log_event['message']

                  slack_message = {
                      'text': f':fire: Erlang Error in {environment}',
                      'attachments': [{
                          'color': 'danger',
                          'fields': [
                              {'title': 'Environment', 'value': environment, 'short': True},
                              {'title': 'Log Stream', 'value': log_data['logStream'], 'short': True},
                              {'title': 'Error', 'value': message[:500], 'short': False}
                          ],
                          'footer': log_data['logGroup'],
                          'ts': log_event['timestamp'] // 1000
                      }]
                  }

                  http.request(
                      'POST',
                      webhook_url,
                      body=json.dumps(slack_message),
                      headers={'Content-Type': 'application/json'}
                  )

              return {'statusCode': 200}

  # IAM Role for Lambda
  ErrorNotifierRole:
    Type: AWS::IAM::Role
    Properties:
      AssumeRolePolicyDocument:
        Version: '2012-10-17'
        Statement:
          - Effect: Allow
            Principal:
              Service: lambda.amazonaws.com
            Action: sts:AssumeRole
      ManagedPolicyArns:
        - arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole

  # Permission for CloudWatch Logs to invoke Lambda
  ErrorNotifierPermission:
    Type: AWS::Lambda::Permission
    Properties:
      FunctionName: !Ref ErrorNotifierFunction
      Action: lambda:InvokeFunction
      Principal: logs.amazonaws.com
      SourceArn: !GetAtt ErrorLogGroup.Arn

Parameters:
  # Add to existing parameters
  SlackWebhookUrl:
    Type: String
    NoEcho: true
    Description: Slack webhook URL for error notifications
```

### Flow

```
Erlang Error
  ↓
Logger → /var/log/hello_erlang/errors.log
  ↓
CloudWatch Agent → CloudWatch Logs
  ↓
Subscription Filter (error level only)
  ↓
Lambda Function
  ↓
Slack Webhook → Channel notification
```

## 4. Local Development Logging

### Option A: File Watching with Desktop Notifications

Create `scripts/watch-errors.sh`:

```bash
#!/bin/bash
# Monitor local error log and send desktop notifications

LOG_FILE="_build/dev/rel/hello_erlang/log/errors.log"

# Create log directory if it doesn't exist
mkdir -p "$(dirname "$LOG_FILE")"
touch "$LOG_FILE"

if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS - use terminal-notifier (install: brew install terminal-notifier)
    echo "Watching $LOG_FILE for errors..."
    tail -F "$LOG_FILE" 2>/dev/null | while read line; do
        if [[ $line == *"error"* ]] || [[ $line == *"crash"* ]]; then
            terminal-notifier -title "Erlang Error" -message "$line" -sound Basso
            echo "🔥 ERROR: $line"
        fi
    done
else
    # Linux - use notify-send
    echo "Watching $LOG_FILE for errors..."
    tail -F "$LOG_FILE" 2>/dev/null | while read line; do
        if [[ $line == *"error"* ]] || [[ $line == *"crash"* ]]; then
            notify-send "Erlang Error" "$line"
            echo "🔥 ERROR: $line"
        fi
    done
fi
```

**Setup:**
```bash
# macOS
brew install terminal-notifier

# Linux
apt-get install libnotify-bin

# Make executable
chmod +x scripts/watch-errors.sh

# Run in background during development
./scripts/watch-errors.sh &
```

### Option B: Local Slack Integration

Create `scripts/local-error-notifier.py`:

```python
#!/usr/bin/env python3
"""
Local error log watcher that sends notifications to Slack
Usage: SLACK_WEBHOOK_URL=https://... python3 scripts/local-error-notifier.py
"""

import time
import requests
import os
import sys
from pathlib import Path

SLACK_WEBHOOK = os.environ.get('SLACK_WEBHOOK_URL')
LOG_FILE = '_build/dev/rel/hello_erlang/log/errors.log'

if not SLACK_WEBHOOK:
    print("Error: SLACK_WEBHOOK_URL environment variable not set")
    sys.exit(1)

def watch_log():
    log_path = Path(LOG_FILE)
    log_path.parent.mkdir(parents=True, exist_ok=True)
    log_path.touch(exist_ok=True)

    print(f"Watching {LOG_FILE} for errors...")

    # Get initial file size
    last_size = log_path.stat().st_size

    while True:
        try:
            current_size = log_path.stat().st_size

            if current_size > last_size:
                # Read new content
                with open(log_path, 'r') as f:
                    f.seek(last_size)
                    new_lines = f.readlines()

                # Check for errors
                for line in new_lines:
                    if 'error' in line.lower() or 'crash' in line.lower():
                        requests.post(SLACK_WEBHOOK, json={
                            'text': ':desktop_computer: Local Erlang Error',
                            'attachments': [{
                                'color': 'warning',
                                'text': f'```{line.strip()}```',
                                'footer': 'Local Development'
                            }]
                        })
                        print(f"🔥 ERROR: {line.strip()}")

                last_size = current_size

            time.sleep(1)

        except KeyboardInterrupt:
            print("\nStopping error watcher...")
            break
        except Exception as e:
            print(f"Error: {e}")
            time.sleep(5)

if __name__ == '__main__':
    watch_log()
```

**Setup:**
```bash
# Install requests (only dependency needed)
pip3 install requests

# Make executable
chmod +x scripts/local-error-notifier.py

# Run during development
SLACK_WEBHOOK_URL="https://hooks.slack.com/..." python3 scripts/local-error-notifier.py &
```

### Option C: Colored Console Logs (Zero Setup)

Use local-specific `sys.config` with colored output:

```erlang
% config/sys.config (for local development)
[
  {kernel, [
    {logger_level, info},
    {logger, [
      {handler, default, logger_std_h, #{
        level => info,
        formatter => {logger_formatter, #{
          single_line => false,
          template => ["\e[36m", time, "\e[0m [", level, "] ", msg, "\n"]
        }}
      }},
      {handler, file_handler, logger_std_h, #{
        level => error,
        config => #{
          file => "log/errors.log",  % Relative path for local
          max_no_bytes => 10485760,
          max_no_files => 5
        }
      }}
    ]}
  ]},
  {hello_erlang, []}
].
```

### Recommended Approach

For local development, use **Option C (colored console)** by default, plus **Option A (desktop notifications)** when you need background monitoring.

## 5. Implementation Checklist

### Phase 1: Basic Logging (Non-invasive)
- [ ] Update `config/sys.config` with logger configuration
- [ ] Add SASL error logging configuration
- [ ] Test locally with `make start` and verify error logs

### Phase 2: EC2 CloudWatch Integration
- [ ] Update `config/aws/stack.yaml` UserData with CloudWatch Agent installation
- [ ] Add CloudWatch Agent configuration for log file monitoring
- [ ] Deploy stack and verify logs appear in CloudWatch

### Phase 3: Slack Notifications
- [ ] Add Lambda function to CloudFormation template
- [ ] Add Subscription Filter to CloudWatch Logs
- [ ] Add `SlackWebhookUrl` parameter to stack creation
- [ ] Test error notification flow

### Phase 4: Local Development
- [ ] Create `scripts/watch-errors.sh` for desktop notifications
- [ ] Create `scripts/local-error-notifier.py` for Slack integration
- [ ] Update development documentation

## 6. Testing Error Notifications

### Generate Test Errors

Add a test endpoint to trigger errors (temporary, for testing):

```erlang
% In echo_handler.erl (just for testing)
init(Req0, State) ->
    case cowboy_req:match_qs([{message, [], <<>>}], Req0) of
        #{message := <<"crash">>} ->
            % Intentional crash for testing
            error(test_error);
        #{message := Message} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                Message,
                Req0),
            {ok, Req, State}
    end.
```

### Test Commands

```bash
# Test local error handling
curl "http://localhost:8080/echo?message=crash"

# Check local error log
cat log/errors.log

# Test EC2 error handling (after deployment)
curl "http://<alb-dns>/echo?message=crash"

# Check CloudWatch Logs
aws logs tail /aws/ec2/hello-erlang/dev --follow

# Verify Slack notification appears
```

## 7. CloudWatch Insights Queries

Once logs are in CloudWatch, use Insights for analysis:

```
# Find all errors in last hour
fields @timestamp, @message
| filter @message like /error/
| sort @timestamp desc
| limit 50

# Error frequency by hour
fields @timestamp, @message
| filter @message like /error/
| stats count() by bin(@timestamp, 1h)

# Specific error patterns
fields @timestamp, @message
| filter @message like /crash/
| parse @message /(?<error_type>\w+):/
| stats count() by error_type
```

## 8. Future Enhancements

### Structured JSON Logging

For better CloudWatch Insights parsing:

```erlang
{handler, file_handler, logger_std_h, #{
  formatter => {logger_formatter, #{
    template => [
      "{\"timestamp\":\"", time,
      "\",\"level\":\"", level,
      "\",\"message\":\"", msg,
      "\"}\n"
    ]
  }}
}}
```

### Request/Response Logging

Add Cowboy middleware for HTTP request logging (still non-invasive):

```erlang
% In hello_erlang_app.erl start/2
cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{
        env => #{dispatch => Dispatch},
        middlewares => [
            cowboy_router,
            hello_erlang_logger_middleware,  % Custom middleware
            cowboy_handler
        ]
    }
).
```

### Metrics Collection

Add Erlang VM metrics to CloudWatch:
- Process count
- Memory usage
- Message queue lengths
- Garbage collection stats

### Alerting

Add CloudWatch Alarms for:
- Error rate thresholds
- Memory usage spikes
- Disk usage warnings
- Health check failures

## References

- [Erlang Logger Documentation](https://www.erlang.org/doc/apps/kernel/logger.html)
- [CloudWatch Agent Configuration](https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Agent-Configuration-File-Details.html)
- [CloudWatch Logs Subscription Filters](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/SubscriptionFilters.html)
- [SASL Error Logging](https://www.erlang.org/doc/apps/sasl/error_logging.html)
