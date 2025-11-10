import json
import os
import urllib.request
import urllib.error
import base64
import gzip
from datetime import datetime

SLACK_WEBHOOK_URL = os.environ.get('SLACK_WEBHOOK_URL', '')
ENVIRONMENT = os.environ.get('ENVIRONMENT', 'unknown')

def handler(event, context):
    """
    Handle Erlang application errors from CloudWatch Logs.

    CloudWatch Logs delivers events in compressed, base64-encoded format.
    """
    print(f"Received event: {json.dumps(event)}")

    if not SLACK_WEBHOOK_URL:
        print("SLACK_WEBHOOK_URL not configured, skipping notification")
        return {'statusCode': 200, 'body': 'Webhook URL not configured'}

    try:
        # Decode compressed CloudWatch Logs data
        compressed_data = base64.b64decode(event['awslogs']['data'])
        log_data = json.loads(gzip.decompress(compressed_data))

        log_group = log_data['logGroup']
        log_stream = log_data['logStream']

        # Send each log event to Slack
        for log_event in log_data['logEvents']:
            message = log_event['message']
            timestamp_ms = log_event['timestamp']

            slack_message = format_slack_message(
                message,
                log_group,
                log_stream,
                timestamp_ms
            )
            send_to_slack(slack_message)

        return {'statusCode': 200, 'body': 'Notifications sent'}

    except Exception as e:
        print(f"Error processing event: {str(e)}")
        raise

def format_slack_message(message, log_group, log_stream, timestamp_ms):
    """Format error as Slack Block Kit message."""

    # Parse timestamp
    try:
        dt = datetime.fromtimestamp(timestamp_ms / 1000)
        time_str = dt.strftime('%Y-%m-%d %H:%M:%S UTC')
    except:
        time_str = str(timestamp_ms)

    # Truncate long messages
    if len(message) > 2000:
        message = message[:2000] + "... (truncated)"

    # Build Slack message using Block Kit
    blocks = [
        {
            "type": "header",
            "text": {
                "type": "plain_text",
                "text": f"🚨 Erlang Application Error"
            }
        },
        {
            "type": "section",
            "fields": [
                {
                    "type": "mrkdwn",
                    "text": f"*Environment:*\n{ENVIRONMENT}"
                },
                {
                    "type": "mrkdwn",
                    "text": f"*Time:*\n{time_str}"
                }
            ]
        },
        {
            "type": "section",
            "text": {
                "type": "mrkdwn",
                "text": f"*Error Message:*\n```{message}```"
            }
        },
        {
            "type": "context",
            "elements": [
                {
                    "type": "mrkdwn",
                    "text": f"Log Stream: `{log_stream}`"
                }
            ]
        }
    ]

    return {
        "attachments": [
            {
                "color": "#d9534f",  # Red/danger
                "blocks": blocks
            }
        ]
    }

def send_to_slack(message):
    """Send formatted message to Slack webhook."""
    try:
        data = json.dumps(message).encode('utf-8')
        req = urllib.request.Request(
            SLACK_WEBHOOK_URL,
            data=data,
            headers={'Content-Type': 'application/json'}
        )

        with urllib.request.urlopen(req, timeout=10) as response:
            print(f"Slack response: {response.status}")

    except urllib.error.HTTPError as e:
        print(f"HTTP Error sending to Slack: {e.code} - {e.reason}")
        print(f"Response: {e.read().decode('utf-8')}")
        raise
    except Exception as e:
        print(f"Error sending to Slack: {str(e)}")
        raise
