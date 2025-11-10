import json
import os
import urllib.request
import urllib.error
from datetime import datetime

SLACK_WEBHOOK_URL = os.environ.get('SLACK_WEBHOOK_URL', '')

def handler(event, context):
    """
    Handle CodePipeline EventBridge events and send notifications to Slack.

    Event structure from EventBridge:
    - detail-type: Pipeline/Stage/Action Execution State Change
    - detail.state: STARTED, SUCCEEDED, FAILED, etc.
    - detail.pipeline: pipeline name
    - detail.stage: stage name (if applicable)
    - detail.action: action name (if applicable)
    """
    print(f"Received event: {json.dumps(event)}")

    if not SLACK_WEBHOOK_URL:
        print("SLACK_WEBHOOK_URL not configured, skipping notification")
        return {'statusCode': 200, 'body': 'Webhook URL not configured'}

    try:
        detail_type = event.get('detail-type', '')
        detail = event.get('detail', {})
        state = detail.get('state', 'UNKNOWN')
        pipeline = detail.get('pipeline', 'unknown-pipeline')
        execution_id = detail.get('execution-id', 'unknown')

        # Calculate duration for completion events
        duration_ms = None
        if state in ['SUCCEEDED', 'FAILED', 'STOPPED']:
            # Get start time from detail and end time from event
            start_time = detail.get('start-time')
            end_time = event.get('time')

            if start_time and end_time:
                try:
                    from datetime import datetime
                    start_dt = datetime.fromisoformat(start_time.replace('Z', '+00:00'))
                    end_dt = datetime.fromisoformat(end_time.replace('Z', '+00:00'))
                    duration_ms = int((end_dt - start_dt).total_seconds() * 1000)
                except Exception as e:
                    print(f"Error calculating duration: {e}")

        # Determine the event scope and build context
        if 'Pipeline Execution' in detail_type:
            scope = 'Pipeline'
            scope_name = pipeline.split('-')[-1]  # Just the environment-specific part
            context = pipeline
        elif 'Stage Execution' in detail_type:
            scope = 'Stage'
            stage = detail.get('stage', 'unknown-stage')
            scope_name = stage
            context = f"{pipeline} → {stage}"
        elif 'Action Execution' in detail_type:
            scope = 'Action'
            stage = detail.get('stage', 'unknown-stage')
            action = detail.get('action', 'unknown-action')
            scope_name = action
            context = f"{pipeline} → {stage} → {action}"
        else:
            scope = 'Unknown'
            scope_name = 'Unknown'
            context = pipeline

        # Format and send Slack message
        slack_message = format_slack_message(scope, scope_name, context, state, detail_type, event.get('time', ''), duration_ms)
        send_to_slack(slack_message)

        return {'statusCode': 200, 'body': 'Notification sent'}

    except Exception as e:
        print(f"Error processing event: {str(e)}")
        raise

def format_slack_message(scope, scope_name, context, state, detail_type, event_time, duration_ms=None):
    """Format EventBridge event as Slack Block Kit message."""

    # Determine message style based on state
    if state in ['FAILED', 'STOPPED', 'SUPERSEDED']:
        emoji = '🚨'
        color = '#d9534f'  # Red for errors
        state_text = f"*{state}*"
    elif state == 'STARTED':
        emoji = '🔄'
        color = '#5bc0de'  # Blue for info
        state_text = state
    elif state == 'SUCCEEDED':
        emoji = '✅'
        color = '#5cb85c'  # Green for success
        state_text = state
    else:
        emoji = 'ℹ️'
        color = '#f0ad4e'  # Orange for other states
        state_text = state

    # Parse timestamp
    try:
        dt = datetime.fromisoformat(event_time.replace('Z', '+00:00'))
        time_str = dt.strftime('%Y-%m-%d %H:%M:%S UTC')
    except:
        time_str = event_time

    # Format context with backticks for better visibility
    context_formatted = context.replace(' → ', ' → `').replace('dev-hello-erlang-pipeline', '`dev-hello-erlang-pipeline`')
    if '`' in context_formatted and not context_formatted.endswith('`'):
        context_formatted = context_formatted + '`'

    # Build title with scope name for better visibility
    if scope == 'Pipeline':
        title = f"{emoji} {scope}: {state_text}"
    else:
        title = f"{emoji} {scope}: {scope_name} → {state_text}"

    # Build fields
    fields = [
        {
            "type": "mrkdwn",
            "text": f"*Context:*\n{context_formatted}"
        },
        {
            "type": "mrkdwn",
            "text": f"*Time:*\n{time_str}"
        }
    ]

    # Add duration if available (will add once we extract it from events)
    if duration_ms:
        duration_sec = duration_ms / 1000
        if duration_sec < 60:
            duration_str = f"{duration_sec:.0f}s"
        else:
            minutes = int(duration_sec // 60)
            seconds = int(duration_sec % 60)
            duration_str = f"{minutes}m {seconds}s"
        fields.append({
            "type": "mrkdwn",
            "text": f"*Duration:*\n{duration_str}"
        })

    # Build Slack message using Block Kit
    blocks = [
        {
            "type": "header",
            "text": {
                "type": "plain_text",
                "text": title
            }
        },
        {
            "type": "section",
            "fields": fields
        },
        {
            "type": "context",
            "elements": [
                {
                    "type": "mrkdwn",
                    "text": f"_{detail_type}_"
                }
            ]
        }
    ]

    return {
        "attachments": [
            {
                "color": color,
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
