# Metrics Viewer Implementation Plan - Erlang Metrics

## Overview

Extends the `/metrics` endpoint to include **Erlang application metrics** alongside EC2 system metrics. Adds a tabbed UI (EC2 tab vs Erlang tab) and integrates Erlang metrics via StatsD protocol.

**Prerequisites:** Complete `metrics-viewer-ec2-implementation-plan.md` first.

## Architecture

```
Erlang App → statsderl (UDP) → CloudWatch Agent (StatsD :8125) → CloudWatch Metrics
                                                                          ↓
                                        Lambda queries metrics ← ← ← ← ← ←
                                                ↓
                                        Returns HTML with tabs
                                                ↓
                                        User sees EC2 + Erlang metrics
```

**Key Components:**
- `statsderl`: Erlang StatsD client library
- CloudWatch Agent: StatsD server (already installed)
- Lambda: Extended to query both EC2 and Erlang metrics
- UI: Bootstrap tabs/pills to separate EC2 vs Erlang

## The 80/20 Erlang Metrics

Based on Pareto principle - these 6-8 metrics provide 80% of observability value:

### Critical Metrics (Must Have)

1. **Process Count**
   - `erlang:system_info(process_count)`
   - Why: Detect process leaks
   - Alert threshold: > 10,000 (unusual for typical app)

2. **Total Memory Usage**
   - `erlang:memory(total)` in bytes
   - Why: Prevent OOM crashes
   - Alert threshold: > 80% of available RAM

3. **Max Message Queue Length**
   - `max([erlang:process_info(P, message_queue_len) || P <- processes()])`
   - Why: Detect backpressure/bottlenecks
   - Alert threshold: > 1,000 messages

### High Value Metrics (Should Have)

4. **Request Count by Endpoint**
   - Track `/echo`, `/add`, etc.
   - Why: Traffic patterns, usage analytics
   - Increment on each request

5. **Request Latency (P50, P95, P99)**
   - Measure handler execution time
   - Why: User experience, SLA tracking
   - Alert: P95 > 500ms

6. **Error Rate**
   - Errors per minute
   - Why: Already have error logs, but rate shows trends
   - Alert: > 10 errors/min

### Optional Metrics (Nice to Have)

7. **Scheduler Utilization**
   - `erlang:statistics(scheduler_wall_time)`
   - Why: Shows CPU saturation
   - Alert: > 80% consistently

8. **Reductions**
   - `erlang:statistics(reductions)`
   - Why: VM work being done
   - Useful for: Comparing load over time

**Metrics to avoid (over-engineering):**
- ❌ Per-process memory (too granular)
- ❌ Per-module function call counts (too many)
- ❌ ETS table sizes (unless known bottleneck)
- ❌ Port count (unless using many ports)

## Implementation Phases

### Phase 1: Enable StatsD in CloudWatch Agent

**File:** `infra/stack.yaml` (EC2 UserData section)

Update the CloudWatch Agent JSON config to add StatsD receiver:

```json
{
  "metrics": {
    "namespace": "HelloErlang",
    "metrics_collected": {
      "statsd": {
        "service_address": ":8125",
        "metrics_collection_interval": 60,
        "metrics_aggregation_interval": 60
      },
      "cpu": { ... existing ... },
      "mem": { ... existing ... },
      "disk": { ... existing ... },
      "netstat": { ... existing ... }
    }
  },
  "logs": { ... existing ... }
}
```

**What this does:**
- CloudWatch Agent listens on UDP port 8125
- Receives StatsD metrics from localhost
- Aggregates and forwards to CloudWatch Metrics
- Same namespace: `HelloErlang`

### Phase 2: Add statsderl Dependency

**File:** `rebar.config`

```erlang
{deps, [
    {cowboy, "2.12.0"},
    {jsone, "1.8.1"},
    {statsderl, "0.5.9"}  % Add StatsD client
]}.
```

**File:** `src/hello_erlang.app.src`

```erlang
{applications, [
    kernel,
    stdlib,
    cowboy,
    jsone,
    statsderl  % Add to applications list
]}.
```

### Phase 3: Create Metrics Module

**File:** `src/hello_erlang_metrics.erl`

```erlang
-module(hello_erlang_metrics).
-export([init/0, report_system_metrics/0, record_request/2, record_latency/2]).

-define(ENABLED, os:getenv("ENABLE_METRICS", "false") =:= "true").

%% Initialize metrics system
init() ->
    case ?ENABLED of
        true ->
            io:format("[Metrics] Enabled, starting statsderl~n"),
            {ok, _} = statsderl:start_link([
                {hostname, "localhost"},
                {port, 8125}
            ]),
            % Report system metrics every 60 seconds
            timer:apply_interval(60000, ?MODULE, report_system_metrics, []),
            ok;
        false ->
            io:format("[Metrics] Disabled~n"),
            ok
    end.

%% Report Erlang VM system metrics
report_system_metrics() ->
    case ?ENABLED of
        true ->
            % Process count
            ProcessCount = erlang:system_info(process_count),
            statsderl:gauge("erlang.processes.count", ProcessCount),

            % Total memory in bytes
            TotalMemory = erlang:memory(total),
            statsderl:gauge("erlang.memory.total_bytes", TotalMemory),

            % Max message queue length across all processes
            MaxQueueLen = find_max_queue_length(),
            statsderl:gauge("erlang.message_queue.max_length", MaxQueueLen),

            % Scheduler utilization (optional)
            % SchedulerUtil = calculate_scheduler_util(),
            % statsderl:gauge("erlang.scheduler.utilization", SchedulerUtil),

            ok;
        false ->
            ok
    end.

%% Record request count for an endpoint
record_request(Handler, StatusCode) ->
    case ?ENABLED of
        true ->
            Metric = lists:flatten(io_lib:format("erlang.requests.~s.~w", [Handler, StatusCode])),
            statsderl:increment(Metric, 1),
            ok;
        false ->
            ok
    end.

%% Record request latency
record_latency(Handler, DurationMs) ->
    case ?ENABLED of
        true ->
            Metric = lists:flatten(io_lib:format("erlang.latency.~s", [Handler])),
            statsderl:timing(Metric, DurationMs),
            ok;
        false ->
            ok
    end.

%% Private functions

find_max_queue_length() ->
    QueueLengths = lists:filtermap(
        fun(P) ->
            case erlang:process_info(P, message_queue_len) of
                {message_queue_len, Len} -> {true, Len};
                undefined -> false
            end
        end,
        erlang:processes()
    ),
    case QueueLengths of
        [] -> 0;
        _ -> lists:max(QueueLengths)
    end.

% calculate_scheduler_util() ->
%     % Implementation for scheduler wall time calculation
%     % Requires tracking previous measurements
%     0.0.
```

**Key features:**
- ✅ Conditional enable/disable via `ENABLE_METRICS` env var
- ✅ UDP fire-and-forget (non-blocking)
- ✅ Graceful degradation if StatsD unavailable
- ✅ No crashes if metrics disabled

### Phase 4: Instrument Application

**File:** `src/hello_erlang_app.erl`

```erlang
start(_StartType, _StartArgs) ->
    % Initialize metrics on app start
    hello_erlang_metrics:init(),

    % ... rest of app startup
    hello_erlang_sup:start_link().
```

**File:** `src/hello_erlang_handler.erl` (example handler)

```erlang
init(Req0, State) ->
    StartTime = erlang:monotonic_time(millisecond),

    % Process request
    {ok, Body} = handle_request(Req0),
    Req = cowboy_req:reply(200, #{}, Body, Req0),

    % Record metrics
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    hello_erlang_metrics:record_request(echo, 200),
    hello_erlang_metrics:record_latency(echo, Duration),

    {ok, Req, State}.
```

**Pattern for all handlers:**
1. Start timer before processing
2. Process request
3. Record request count (with status code)
4. Record latency

### Phase 5: Update CloudFormation Stack

**File:** `infra/stack.yaml` (EC2 UserData)

Add environment variable to enable metrics:

```yaml
UserData:
  Fn::Base64: !Sub |
    #!/bin/bash
    set -e

    # ... existing setup ...

    # Enable metrics for Erlang app
    export ENABLE_METRICS=true

    # ... rest of UserData ...
```

**Ensure environment persists:**
Add to `/etc/environment` or systemd service file so metrics remain enabled after app restarts.

### Phase 6: Update Lambda Handler

**File:** `infra/handlers/metrics_viewer/index.py`

Extend to query Erlang metrics alongside EC2 metrics:

```python
def get_erlang_metrics(environment, instance_ids, hours=6):
    """
    Query CloudWatch for Erlang-specific metrics.
    """
    cloudwatch = boto3.client('cloudwatch')
    end_time = datetime.utcnow()
    start_time = end_time - timedelta(hours=hours)

    metrics = {
        'processes': [],
        'memory': [],
        'message_queues': [],
        'requests': {},
        'latency': {}
    }

    # Process count
    metrics['processes'] = get_metric_data(
        'HelloErlang',
        'erlang.processes.count',
        instance_ids[0],  # or aggregate all instances
        hours
    )

    # Memory usage
    metrics['memory'] = get_metric_data(
        'HelloErlang',
        'erlang.memory.total_bytes',
        instance_ids[0],
        hours
    )

    # Message queue length
    metrics['message_queues'] = get_metric_data(
        'HelloErlang',
        'erlang.message_queue.max_length',
        instance_ids[0],
        hours
    )

    # Request counts per endpoint
    for handler in ['echo', 'add']:
        metric_name = f'erlang.requests.{handler}.200'
        metrics['requests'][handler] = get_metric_data(
            'HelloErlang',
            metric_name,
            instance_ids[0],
            hours
        )

    # Latency (if using statsderl:timing)
    # StatsD timing creates percentile metrics: p50, p95, p99

    return metrics

def handler(event, context):
    environment = os.environ['ENVIRONMENT']
    instance_ids = get_instance_ids(environment)

    # Get both EC2 and Erlang metrics
    ec2_metrics = get_ec2_metrics(environment, instance_ids)
    erlang_metrics = get_erlang_metrics(environment, instance_ids)

    # Load template and inject data
    template = load_template()
    html = template.replace('{{EC2_METRICS_JSON}}', json.dumps(ec2_metrics))
    html = html.replace('{{ERLANG_METRICS_JSON}}', json.dumps(erlang_metrics))
    html = html.replace('{{ENVIRONMENT}}', environment)
    html = html.replace('{{INSTANCE_IDS}}', ', '.join(instance_ids))

    return {
        'statusCode': 200,
        'headers': {'Content-Type': 'text/html'},
        'body': html
    }
```

### Phase 7: Update HTML Template with Tabs

**File:** `infra/handlers/metrics_viewer/template.html`

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Hello Erlang - Metrics Dashboard</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chartjs-adapter-date-fns@3.0.0/dist/chartjs-adapter-date-fns.bundle.min.js"></script>
</head>
<body>
    <div class="container mt-4">
        <h1>Hello Erlang Metrics - {{ENVIRONMENT}}</h1>
        <p class="text-muted">Last 6 hours | Auto-refresh: 60s</p>

        <!-- Nav Pills/Tabs -->
        <ul class="nav nav-pills mb-4" id="metrics-tabs" role="tablist">
            <li class="nav-item" role="presentation">
                <button class="nav-link active" id="ec2-tab" data-bs-toggle="pill"
                        data-bs-target="#ec2-panel" type="button" role="tab">
                    <strong>EC2 Metrics</strong>
                </button>
            </li>
            <li class="nav-item" role="presentation">
                <button class="nav-link" id="erlang-tab" data-bs-toggle="pill"
                        data-bs-target="#erlang-panel" type="button" role="tab">
                    <strong>Erlang Metrics</strong>
                </button>
            </li>
        </ul>

        <!-- Tab Content -->
        <div class="tab-content" id="metrics-tab-content">

            <!-- EC2 Panel -->
            <div class="tab-pane fade show active" id="ec2-panel" role="tabpanel">
                <div class="row">
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-primary text-white">CPU Usage (%)</div>
                            <div class="card-body">
                                <canvas id="cpuChart"></canvas>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-primary text-white">Memory Usage (%)</div>
                            <div class="card-body">
                                <canvas id="memoryChart"></canvas>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="row">
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-primary text-white">Disk Usage (%)</div>
                            <div class="card-body">
                                <canvas id="diskChart"></canvas>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-primary text-white">TCP Connections</div>
                            <div class="card-body">
                                <canvas id="tcpChart"></canvas>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Erlang Panel -->
            <div class="tab-pane fade" id="erlang-panel" role="tabpanel">
                <div class="row">
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-success text-white">Process Count</div>
                            <div class="card-body">
                                <canvas id="processCountChart"></canvas>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-success text-white">Memory Usage (MB)</div>
                            <div class="card-body">
                                <canvas id="erlangMemoryChart"></canvas>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="row">
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-success text-white">Message Queue Length (Max)</div>
                            <div class="card-body">
                                <canvas id="messageQueueChart"></canvas>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="card mb-4">
                            <div class="card-header bg-success text-white">Request Rate (req/min)</div>
                            <div class="card-body">
                                <canvas id="requestRateChart"></canvas>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

        </div>

        <!-- Instance Info -->
        <div class="alert alert-info mt-4">
            <strong>Instances:</strong> {{INSTANCE_IDS}}
        </div>
    </div>

    <script>
        // Data from Lambda
        const ec2Metrics = {{EC2_METRICS_JSON}};
        const erlangMetrics = {{ERLANG_METRICS_JSON}};

        // Chart.js default config
        const chartConfig = {
            type: 'line',
            options: {
                responsive: true,
                plugins: {
                    legend: { display: true }
                },
                scales: {
                    x: {
                        type: 'time',
                        time: {
                            unit: 'minute',
                            displayFormats: {
                                minute: 'HH:mm'
                            }
                        }
                    },
                    y: {
                        beginAtZero: true
                    }
                }
            }
        };

        // EC2 Charts
        new Chart(document.getElementById('cpuChart'), {
            ...chartConfig,
            data: ec2Metrics.cpu
        });

        new Chart(document.getElementById('memoryChart'), {
            ...chartConfig,
            data: ec2Metrics.memory
        });

        new Chart(document.getElementById('diskChart'), {
            ...chartConfig,
            data: ec2Metrics.disk
        });

        new Chart(document.getElementById('tcpChart'), {
            ...chartConfig,
            data: ec2Metrics.tcp
        });

        // Erlang Charts
        new Chart(document.getElementById('processCountChart'), {
            ...chartConfig,
            data: erlangMetrics.processes
        });

        new Chart(document.getElementById('erlangMemoryChart'), {
            ...chartConfig,
            data: erlangMetrics.memory,
            options: {
                ...chartConfig.options,
                scales: {
                    ...chartConfig.options.scales,
                    y: {
                        beginAtZero: true,
                        ticks: {
                            // Convert bytes to MB
                            callback: function(value) {
                                return (value / 1024 / 1024).toFixed(2) + ' MB';
                            }
                        }
                    }
                }
            }
        });

        new Chart(document.getElementById('messageQueueChart'), {
            ...chartConfig,
            data: erlangMetrics.message_queues
        });

        new Chart(document.getElementById('requestRateChart'), {
            ...chartConfig,
            data: erlangMetrics.requests
        });

        // Auto-refresh every 60 seconds
        setTimeout(() => location.reload(), 60000);
    </script>
</body>
</html>
```

**UI Features:**
- ✅ Bootstrap 5 tabs/pills for EC2 vs Erlang
- ✅ Color-coded: Blue for EC2, Green for Erlang
- ✅ 4 charts per tab (2x2 grid)
- ✅ Auto-refresh every 60 seconds
- ✅ Responsive design

## Testing Plan

### Phase 1: Local Development Testing

**Test metrics disabled:**
```bash
# In local dev (no ENABLE_METRICS env var)
rebar3 shell

# Verify metrics module doesn't crash
> hello_erlang_metrics:init().
[Metrics] Disabled
ok

> hello_erlang_metrics:record_request(echo, 200).
ok  % Should do nothing, no crash
```

### Phase 2: StatsD Integration Testing

**Test metrics enabled:**
```bash
# Start local StatsD server (for testing)
docker run -d -p 8125:8125/udp graphiteapp/graphite-statsd

# Run Erlang app with metrics enabled
ENABLE_METRICS=true rebar3 shell

# Verify metrics sending
> hello_erlang_metrics:init().
[Metrics] Enabled, starting statsderl
ok

> hello_erlang_metrics:report_system_metrics().
ok

# Check StatsD server logs for received metrics
```

### Phase 3: AWS CloudWatch Agent Testing

1. Deploy updated stack to dev
2. SSH to EC2 via SSM: `aws ssm start-session --target <instance-id>`
3. Check CloudWatch Agent status:
   ```bash
   sudo /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl -a query -m ec2 -c default
   ```
4. Check StatsD port is listening:
   ```bash
   sudo netstat -ulnp | grep 8125
   ```
5. Test metrics sending:
   ```bash
   echo "test.metric:1|c" | nc -u -w1 localhost 8125
   ```
6. Check CloudWatch Console: Metrics → HelloErlang → erlang.*

### Phase 4: End-to-End Testing

1. Visit `https://<alb-url>/metrics`
2. Verify EC2 tab shows 4 charts with data
3. Click Erlang tab
4. Verify Erlang tab shows 4 charts with data
5. Generate traffic: `curl https://<alb-url>/echo?msg=test`
6. Wait 60 seconds for metrics to update
7. Refresh `/metrics` page, verify request count increased

## Cost Analysis

**CloudWatch Metrics:**
- First 10 custom metrics: Free
- EC2 metrics: 4 (CPU, memory, disk, TCP)
- Erlang metrics: ~8 (processes, memory, queues, requests x2, latency x2)
- Total: ~12 metrics

**Beyond free tier:** 2 metrics × $0.30/month = **$0.60/month**

**Lambda:** No change (same function, slightly more CloudWatch API calls)

**Total additional cost: ~$0.60/month**

## Rollback Plan

1. **Disable metrics in Erlang app:**
   - Set `ENABLE_METRICS=false` in CloudFormation UserData
   - Redeploy EC2 instances
   - Erlang stops sending metrics

2. **Remove StatsD from CloudWatch Agent:**
   - Remove `statsd` section from agent config
   - Restart CloudWatch Agent
   - Agent stops listening on port 8125

3. **Revert Lambda to EC2-only:**
   - Update Lambda to only query EC2 metrics
   - Remove Erlang tab from HTML template

4. **Remove statsderl dependency:**
   - Remove from `rebar.config`
   - Remove metrics module
   - Rebuild release

## Future Enhancements

### Priority 1: Request Latency Percentiles
- StatsD timing metrics give P50, P95, P99 automatically
- Add latency charts to Erlang tab

### Priority 2: Error Rate Tracking
- Count errors by type/handler
- Add error rate chart
- Alert on spikes

### Priority 3: Alerts
- CloudWatch Alarms for critical thresholds
- Process count > 10,000
- Message queue > 1,000
- Memory > 80%
- SNS → Slack (similar to existing error notifications)

### Priority 4: Multi-Instance Aggregation
- If scaling to multiple EC2 instances
- Aggregate metrics across all instances
- Show per-instance breakdown (expandable)

### Priority 5: Custom Time Ranges
- Add date picker to UI
- Query parameters: `/metrics?hours=24`
- Show last 1h, 6h, 24h, 7d

## Dependencies Summary

**New Erlang Dependencies:**
```erlang
{deps, [
    {statsderl, "0.5.9"}
]}.
```

**New Files:**
- `src/hello_erlang_metrics.erl` - Metrics collection module
- Updated: `src/hello_erlang_app.erl` - Initialize metrics
- Updated: Handler files - Instrument with metrics calls

**CloudFormation Changes:**
- EC2 UserData: Enable StatsD in CloudWatch Agent
- EC2 UserData: Set `ENABLE_METRICS=true` environment variable
- Lambda: Extended to query Erlang metrics
- HTML Template: Add tabs/pills UI

**No new AWS resources needed** - uses existing infrastructure!

## Success Criteria

- ✅ Erlang metrics visible in CloudWatch Console under HelloErlang namespace
- ✅ `/metrics` endpoint shows tabbed UI (EC2 vs Erlang)
- ✅ Erlang tab shows 4-6 charts with live data
- ✅ Request counts increment when traffic generated
- ✅ Process count, memory, queue length update every 60s
- ✅ Metrics can be disabled in dev (no crashes)
- ✅ Total cost < $1/month
- ✅ Page load time < 3 seconds
- ✅ No performance impact on request handlers

## Questions for User

1. **Latency tracking:** Want P50/P95/P99 latency charts, or just request count for now?
2. **Error tracking:** Include error rate charts, or rely on existing error logging?
3. **Multiple handlers:** Track metrics per-handler (echo, add separately) or aggregate?
4. **Scheduler metrics:** Include scheduler utilization, or skip for simplicity?
5. **Time window:** 6 hours sufficient, or need longer retention in dashboard?
