# Metrics Viewer Implementation Plan

## Overview

Add a `/metrics` endpoint to the ALB that displays EC2 system metrics (CPU, memory, disk) in a web dashboard. Uses CloudWatch Agent for collection, Lambda for serving, and Bootstrap + Chart.js for visualization.

## Architecture

```
User → ALB (/metrics) → Lambda → CloudWatch API → Returns HTML with graphs
                                ↓
                            EC2 API (get instance IDs)
```

**Key Design Decisions:**
- No hardcoded instance IDs - dynamic lookup via EC2 tags
- HTML template separate from handler code
- Bootstrap + Chart.js loaded from CDN
- Single Lambda handler, no API Gateway needed

## Implementation Phases

### Phase 1: Update CloudWatch Agent Configuration

**File:** `infra/stack.yaml`

**Changes to EC2 UserData:**

Update the CloudWatch Agent JSON config to include metrics collection:

```json
{
  "metrics": {
    "namespace": "HelloErlang",
    "metrics_collected": {
      "cpu": {
        "measurement": [
          {"name": "cpu_usage_idle", "rename": "CPU_IDLE", "unit": "Percent"},
          {"name": "cpu_usage_iowait", "rename": "CPU_IOWAIT", "unit": "Percent"}
        ],
        "metrics_collection_interval": 60,
        "totalcpu": false
      },
      "mem": {
        "measurement": [
          {"name": "mem_used_percent", "rename": "MEMORY_USED", "unit": "Percent"}
        ],
        "metrics_collection_interval": 60
      },
      "disk": {
        "measurement": [
          {"name": "disk_used_percent", "rename": "DISK_USED", "unit": "Percent"}
        ],
        "resources": ["*"],
        "metrics_collection_interval": 60
      },
      "netstat": {
        "measurement": [
          {"name": "tcp_established", "rename": "TCP_CONNECTIONS", "unit": "Count"}
        ],
        "metrics_collection_interval": 60
      }
    }
  },
  "logs": {
    ... existing logs config ...
  }
}
```

**Metrics to collect (the 20% that gives 80% value):**
- CPU: idle %, iowait % (calculate usage = 100 - idle)
- Memory: used %
- Disk: used % (all mounted filesystems)
- Network: established TCP connections

### Phase 2: Create Lambda Handler

**File:** `infra/handlers/metrics_viewer/index.py`

**Responsibilities:**
1. Lookup EC2 instance IDs dynamically by stack/environment tags
2. Query CloudWatch API for last 6 hours of metrics
3. Format data for Chart.js
4. Load HTML template and inject data
5. Return HTML response

**Key Functions:**

```python
def get_instance_ids(environment):
    """
    Query EC2 API for instances tagged with:
    - Environment={environment}
    - Application=hello-erlang
    Returns list of instance IDs.
    """

def get_metric_data(namespace, metric_name, instance_id, hours=6):
    """
    Query CloudWatch for metric data.
    Returns list of {timestamp, value} dicts.
    """

def handler(event, context):
    """
    Main Lambda handler.
    1. Extract environment from ALB request
    2. Get instance IDs
    3. Fetch metrics for all instances
    4. Load template
    5. Inject data as JSON
    6. Return HTML response
    """
```

**ALB Integration:**
Lambda must return response in ALB-compatible format:

```python
return {
    'statusCode': 200,
    'statusDescription': '200 OK',
    'headers': {
        'Content-Type': 'text/html',
    },
    'body': html_content
}
```

**Environment Variable:**
- `ENVIRONMENT`: dev/staging/prod (set by CloudFormation)

### Phase 3: Create HTML Template

**File:** `infra/handlers/metrics_viewer/template.html`

**Structure:**

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Hello Erlang - Metrics Dashboard</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
</head>
<body>
    <div class="container mt-4">
        <h1>Hello Erlang Metrics - {{ENVIRONMENT}}</h1>
        <p class="text-muted">Last 6 hours | Auto-refresh: 60s</p>

        <div class="row">
            <div class="col-md-6">
                <div class="card mb-4">
                    <div class="card-header">CPU Usage</div>
                    <div class="card-body">
                        <canvas id="cpuChart"></canvas>
                    </div>
                </div>
            </div>
            <div class="col-md-6">
                <div class="card mb-4">
                    <div class="card-header">Memory Usage</div>
                    <div class="card-body">
                        <canvas id="memoryChart"></canvas>
                    </div>
                </div>
            </div>
        </div>

        <div class="row">
            <div class="col-md-6">
                <div class="card mb-4">
                    <div class="card-header">Disk Usage</div>
                    <div class="card-body">
                        <canvas id="diskChart"></canvas>
                    </div>
                </div>
            </div>
            <div class="col-md-6">
                <div class="card mb-4">
                    <div class="card-header">TCP Connections</div>
                    <div class="card-body">
                        <canvas id="tcpChart"></canvas>
                    </div>
                </div>
            </div>
        </div>

        <div class="alert alert-info">
            <strong>Instances:</strong> {{INSTANCE_IDS}}
        </div>
    </div>

    <script>
        const metricsData = {{METRICS_JSON}};

        // Chart.js configuration
        const chartConfig = {
            type: 'line',
            options: {
                responsive: true,
                plugins: {
                    legend: { display: true },
                },
                scales: {
                    x: { type: 'time' },
                    y: { beginAtZero: true, max: 100 }
                }
            }
        };

        // Create charts
        new Chart(document.getElementById('cpuChart'), {
            ...chartConfig,
            data: metricsData.cpu
        });

        new Chart(document.getElementById('memoryChart'), {
            ...chartConfig,
            data: metricsData.memory
        });

        new Chart(document.getElementById('diskChart'), {
            ...chartConfig,
            data: metricsData.disk
        });

        new Chart(document.getElementById('tcpChart'), {
            ...chartConfig,
            data: metricsData.tcp
        });

        // Auto-refresh every 60 seconds
        setTimeout(() => location.reload(), 60000);
    </script>
</body>
</html>
```

**Template Variables:**
- `{{ENVIRONMENT}}`: dev/staging/prod
- `{{INSTANCE_IDS}}`: Comma-separated instance IDs
- `{{METRICS_JSON}}`: JSON object with chart data

### Phase 4: Update CloudFormation Stack

**File:** `infra/stack.yaml`

#### 4.1 Add Lambda Function

```yaml
MetricsViewerFunction:
  Type: AWS::Lambda::Function
  Properties:
    FunctionName: !Sub '${Environment}-hello-erlang-metrics-viewer'
    Runtime: python3.11
    Handler: metrics_viewer.index.handler
    Role: !GetAtt MetricsViewerRole.Arn
    Timeout: 30
    Code:
      S3Bucket: !Sub '${Environment}-hello-erlang-stack-artifacts-${AWS::AccountId}'
      S3Key: handlers.zip
    Environment:
      Variables:
        ENVIRONMENT: !Ref Environment
    Tags:
      - Key: Environment
        Value: !Ref Environment
      - Key: Application
        Value: hello-erlang
```

#### 4.2 Add IAM Role for Lambda

```yaml
MetricsViewerRole:
  Type: AWS::IAM::Role
  Properties:
    RoleName: !Sub '${Environment}-hello-erlang-metrics-viewer-role'
    AssumeRolePolicyDocument:
      Version: '2012-10-17'
      Statement:
        - Effect: Allow
          Principal:
            Service: lambda.amazonaws.com
          Action: sts:AssumeRole
    ManagedPolicyArns:
      - arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole
    Policies:
      - PolicyName: MetricsViewerPolicy
        PolicyDocument:
          Version: '2012-10-17'
          Statement:
            - Effect: Allow
              Action:
                - cloudwatch:GetMetricStatistics
                - cloudwatch:ListMetrics
              Resource: '*'
            - Effect: Allow
              Action:
                - ec2:DescribeInstances
              Resource: '*'
```

#### 4.3 Add Lambda Target Group

```yaml
MetricsViewerTargetGroup:
  Type: AWS::ElasticLoadBalancingV2::TargetGroup
  DependsOn: MetricsViewerInvokePermission
  Properties:
    Name: !Sub '${Environment}-metrics-viewer'
    TargetType: lambda
    Targets:
      - Id: !GetAtt MetricsViewerFunction.Arn
    Tags:
      - Key: Environment
        Value: !Ref Environment
      - Key: Application
        Value: hello-erlang
```

#### 4.4 Add Lambda Invoke Permission

```yaml
MetricsViewerInvokePermission:
  Type: AWS::Lambda::Permission
  Properties:
    FunctionName: !Ref MetricsViewerFunction
    Action: lambda:InvokeFunction
    Principal: elasticloadbalancing.amazonaws.com
    SourceArn: !Sub 'arn:aws:elasticloadbalancing:${AWS::Region}:${AWS::AccountId}:targetgroup/${Environment}-metrics-viewer/*'
```

#### 4.5 Add ALB Listener Rule

```yaml
MetricsListenerRule:
  Type: AWS::ElasticLoadBalancingV2::ListenerRule
  Properties:
    Actions:
      - Type: forward
        TargetGroupArn: !Ref MetricsViewerTargetGroup
    Conditions:
      - Field: path-pattern
        Values:
          - /metrics
    ListenerArn: !Ref ALBListener
    Priority: 10
```

**Note:** Existing default rule (priority 100+) forwards all other traffic to EC2.

### Phase 5: Update Deployment Script

**File:** `scripts/aws/deploy-stack.sh`

**Changes:**
No changes needed! The `package_and_upload_handlers()` function already recursively zips all handlers, so `metrics_viewer/` will be included automatically.

**Verification:**
After deployment, check that handlers.zip contains:
- `app_error_notifier/index.py`
- `pipeline_notifier/index.py`
- `metrics_viewer/index.py`
- `metrics_viewer/template.html`

### Phase 6: Update Documentation

**File:** `infra/README.md`

Add section:

```markdown
## Metrics Dashboard

View system metrics at: `https://<your-alb-url>/metrics`

**Displays:**
- CPU usage (last 6 hours)
- Memory usage
- Disk usage
- TCP connections
- Auto-refreshes every 60 seconds

**Data source:** CloudWatch Agent on EC2 instances
**Namespace:** HelloErlang
**Collection interval:** 60 seconds
```

## Testing Plan

### Phase 1: CloudWatch Agent
1. Deploy updated stack
2. SSH to EC2: `aws ssm start-session --target <instance-id>`
3. Verify agent running: `sudo /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl -a query -m ec2 -c default`
4. Check CloudWatch Console: Metrics → HelloErlang namespace
5. Wait 2-3 minutes for first data points

### Phase 2: Lambda Function
1. Test Lambda directly via AWS Console
2. Check CloudWatch Logs for Lambda execution logs
3. Verify it can find EC2 instances by tags
4. Verify it can query metrics from CloudWatch

### Phase 3: ALB Integration
1. Visit `https://<alb-url>/metrics`
2. Verify page loads (Bootstrap styling visible)
3. Verify charts render (Chart.js working)
4. Verify data shows in charts
5. Wait 60 seconds, verify auto-refresh works

### Phase 4: Multi-instance
1. If/when you scale to multiple EC2 instances
2. Verify all instances appear in metrics
3. Verify charts aggregate or separate data appropriately

## Cost Analysis

**CloudWatch Metrics:**
- First 10 custom metrics: Free
- We're collecting 4 metrics: Free tier ✓
- Beyond 10 metrics: $0.30/metric/month

**Lambda:**
- Invocations: ~1,440/month (if viewed once/hour) = Free tier ✓
- Compute: 30 seconds @ 128MB = negligible

**ALB:**
- No additional cost (already have ALB)

**Total additional cost: ~$0/month** (within free tier)

## Security Considerations

### Option A: Public Access (Current Plan)
- `/metrics` endpoint publicly accessible
- No authentication
- Only shows aggregated metrics (no sensitive data)
- **Risk:** Low - metrics are not sensitive

### Option B: Add Basic Auth (Future Enhancement)
- Lambda checks `Authorization` header
- Compare against environment variable
- Return 401 if unauthorized
- **Implementation:** Add `METRICS_PASSWORD` env var, check in handler

### Option C: VPN/IP Whitelist (Enterprise)
- ALB security group restricts `/metrics` access
- Only from office/VPN IPs
- **Implementation:** Add security group rule

**Recommendation:** Start with Option A, add Option B if needed.

## Future Enhancements

### Priority 1: Add Erlang-Specific Metrics
- Erlang process count
- Message queue lengths
- Erlang VM memory breakdown
- Scheduler utilization

**Implementation:**
- Add custom metrics to Erlang app
- Use `erlang-cloudwatch` library or custom StatsD exporter
- CloudWatch Agent ingests via StatsD protocol
- Update Lambda to query new metrics

### Priority 2: Historical Data
- Add date range picker (last 1h, 6h, 24h, 7d)
- Use query parameters: `/metrics?hours=24`

### Priority 3: Alerts
- CloudWatch Alarms for high CPU/memory
- SNS → Slack notifications
- Similar to existing error notifications

### Priority 4: Export/Download
- Add "Download CSV" button
- Lambda generates CSV from metrics data

## Rollout Strategy

1. **Deploy to dev first**
   - Test all functionality
   - Verify metrics collection
   - Iterate on chart styling

2. **Production deployment**
   - Same as dev (no environment-specific code)
   - Monitor Lambda execution for errors
   - Check CloudWatch costs after 1 week

3. **Rollback plan**
   - Remove ALB listener rule (stops routing to Lambda)
   - Delete Lambda function and target group
   - Remove metrics collection from CloudWatch Agent config
   - No data loss, no downtime

## Success Criteria

- ✅ EC2 metrics visible in CloudWatch Console
- ✅ `/metrics` endpoint returns HTML with Bootstrap styling
- ✅ Charts render with Chart.js
- ✅ Data refreshes automatically
- ✅ Works for all environments (dev/staging/prod)
- ✅ No additional costs beyond free tier
- ✅ Page load time < 3 seconds

## Questions for User

1. **Chart granularity:** 60-second intervals OK, or need 5-minute aggregation?
2. **Data retention:** 6 hours visible in dashboard sufficient?
3. **Auto-refresh:** 60 seconds OK, or configurable?
4. **Chart style:** Line charts for all metrics, or bar/area for some?
5. **Multiple instances:** If you scale EC2, show aggregated or per-instance?
