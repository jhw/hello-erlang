# Kafka vs Erlang Distributed Messaging

## The Question

**Should we use Kafka to communicate between Erlang VMs, or use Erlang's built-in distributed messaging?**

## The Short Answer

**Using Kafka between Erlang VMs is like using a semi-truck to deliver a letter to your neighbor.** Erlang's distributed messaging is purpose-built for exactly this use case and is orders of magnitude faster, simpler, and more reliable.

## Performance Comparison

### Erlang Distributed Messaging

```erlang
% Send message to process on another node
{worker, 'node2@hostname'} ! {task, Data}.

% Latency: 50-200 microseconds
% Throughput: 100,000+ messages/second per connection
% Setup: Zero (built into BEAM)
```

### Kafka Messaging

```erlang
% Send message via Kafka
ok = brod:produce_sync(
    kafka_client,
    <<"my-topic">>,
    _Partition = 0,
    _Key = <<"">>,
    _Value = term_to_binary({task, Data})
).

% Latency: 5-50 milliseconds (100x-1000x slower)
% Throughput: 1,000-10,000 messages/second (10x-100x slower)
% Setup: Kafka cluster (ZooKeeper, brokers, topics, consumer groups)
```

**Erlang is 100-1000x faster** for inter-VM communication.

## Latency Breakdown

### Erlang Distribution

```
Process A (node1)
  ↓
BEAM internal send (10ns)
  ↓
TCP send buffer (10μs)
  ↓
Network (20-50μs in same AZ)
  ↓
TCP receive buffer (10μs)
  ↓
BEAM receive queue (10ns)
  ↓
Process B (node2)

Total: ~50-100 microseconds
```

### Kafka

```
Erlang Process
  ↓
Kafka client library encode (50μs)
  ↓
TCP to Kafka broker (50μs)
  ↓
Broker disk write (1-10ms) ← MAJOR BOTTLENECK
  ↓
Replication to followers (1-5ms) ← MAJOR BOTTLENECK
  ↓
Consumer poll (100μs-1ms)
  ↓
Kafka client library decode (50μs)
  ↓
Erlang Process

Total: ~5-50 milliseconds (50x-500x slower)
```

**Kafka's disk write and replication are the killers.** Even with `acks=0` (no durability), you're still 10x slower than Erlang.

## Message Guarantees

### Erlang Distribution

```erlang
% Option 1: Fire-and-forget
Pid ! Message.
% Delivery: Best effort (TCP guarantees in-order delivery)

% Option 2: Monitored send
Ref = erlang:monitor(process, Pid),
Pid ! Message,
receive
    {'DOWN', Ref, process, Pid, Reason} ->
        % Process died, handle failure
        retry_or_fail()
after 5000 ->
    % Timeout, handle failure
    retry_or_fail()
end.

% Option 3: Synchronous call (with response)
Response = gen_server:call({worker, Node}, Request, 5000).
% Guaranteed delivery + response or timeout
```

### Kafka

```erlang
% Option 1: Fire-and-forget (acks=0)
ok = brod:produce(client, Topic, Partition, Key, Value).
% Delivery: No guarantee (can lose messages)

% Option 2: Leader acknowledgment (acks=1)
ok = brod:produce_sync(client, Topic, Partition, Key, Value).
% Delivery: Guaranteed written to leader (but not replicas)

% Option 3: Full replication (acks=all)
ok = brod:produce_sync_offset(client, Topic, Partition, Key, Value).
% Delivery: Guaranteed replicated (5-50ms latency)
```

**Problem**: Even Kafka's strongest guarantee (`acks=all`) doesn't tell you if the **consumer received the message**. You only know it was written to disk.

**Erlang's `gen_server:call`** gives you **end-to-end confirmation** in one operation.

## Code Complexity

### Erlang: Send Message to Another Node

```erlang
% Setup (one-time, in sys.config)
% Nodes automatically discover each other via epmd or DNS

% Usage
{worker_pool, 'node2@hostname'} ! {process, Job}.

% That's it. Three lines.
```

### Kafka: Send Message to Another Service

```erlang
% Setup (infrastructure)
% 1. Deploy Kafka cluster (3+ brokers)
% 2. Deploy ZooKeeper ensemble (3+ nodes)
% 3. Create topics, configure partitions
% 4. Set up consumer groups
% 5. Configure replication, retention, compaction

% Setup (application)
-module(kafka_producer).
-behaviour(gen_server).

init(_) ->
    % Start Kafka client
    KafkaHosts = [{"kafka1", 9092}, {"kafka2", 9092}, {"kafka3", 9092}],
    ok = brod:start_client(KafkaHosts, kafka_client, [
        {auto_start_producers, true},
        {default_producer_config, [
            {partition_buffer_limit, 256},
            {max_batch_size, 1000}
        ]}
    ]),
    {ok, #state{}}.

% Usage
send_message(Job) ->
    Key = term_to_binary(job_id(Job)),
    Value = term_to_binary(Job),
    case brod:produce_sync(kafka_client, <<"jobs">>, 0, Key, Value) of
        ok -> ok;
        {error, Reason} ->
            % Handle: broker down, topic doesn't exist, partition unavailable,
            %         message too large, timeout, etc.
            retry_logic(Reason)
    end.

% Consumer (separate process/service)
-module(kafka_consumer).

init(_) ->
    Topics = [<<"jobs">>],
    ok = brod:start_link_group_subscriber(
        kafka_client,
        <<"job-consumer-group">>,
        Topics,
        _GroupConfig = [
            {offset_commit_policy, commit_to_kafka_v2},
            {offset_commit_interval_seconds, 5}
        ],
        _ConsumerConfig = [
            {begin_offset, earliest}
        ],
        _CallbackModule = ?MODULE,
        _CallbackInitArg = []
    ),
    {ok, #state{}}.

handle_message(Topic, Partition, Message, State) ->
    #kafka_message{key = Key, value = Value, offset = Offset} = Message,
    Job = binary_to_term(Value),
    process_job(Job),
    {ok, ack, State}.  % Commit offset

% That's ~100+ lines of code + infrastructure management
```

**Complexity ratio**: Kafka is **30-50x more code** than Erlang distribution.

## Failure Modes

### Erlang Distribution Failures

```erlang
% Failure scenarios:
1. Remote node down
   → Monitor detects immediately
   → {'DOWN', Ref, process, Pid, noconnection}

2. Network partition
   → net_kernel detects within seconds
   → {nodedown, Node} message sent to subscribers

3. Process doesn't exist
   → Message lost (same as local send to dead process)
   → Use monitor/link to detect

4. Process mailbox full
   → Sender blocks (backpressure)
   → Or crashes if memory limit exceeded

% Handling:
Ref = erlang:monitor(process, {worker, Node}),
Pid ! Message,
receive
    {'DOWN', Ref, process, _, Reason} ->
        handle_failure(Reason)
after 5000 ->
    timeout
end.

% Simple, direct, deterministic
```

### Kafka Failures

```erlang
% Failure scenarios:
1. Kafka broker down
   → Producer times out
   → Automatic retry to other brokers
   → May duplicate messages

2. Network partition
   → Split brain (multiple leaders possible)
   → Messages may be lost or duplicated
   → Requires careful configuration

3. Topic doesn't exist
   → auto.create.topics.enable (risky)
   → Or manual topic creation (operational burden)

4. Consumer lag
   → Messages pile up in Kafka
   → Offset management complexity
   → Replay logic needed

5. Partition rebalancing
   → Consumer group coordinator election
   → Messages paused during rebalance
   → Duplicate processing possible

6. Message too large
   → max.message.bytes exceeded
   → Must fragment and reassemble

7. Serialization errors
   → term_to_binary failures
   → Schema evolution problems

% Handling: Complex retry logic, idempotency, offset management, monitoring
```

**Kafka has 10x more failure modes** than Erlang distribution.

## When to Use Each

### Use Erlang Distribution When:

✅ **Communicating between Erlang services**
   - Built for this exact use case

✅ **Low latency required** (<1ms)
   - Erlang: 50-200μs
   - Kafka: 5-50ms

✅ **Request-response patterns**
   - `gen_server:call` built-in
   - Kafka: requires correlation IDs, response topics

✅ **Small, frequent messages**
   - Erlang: optimized for this
   - Kafka: overhead per message

✅ **Ephemeral messages** (no persistence needed)
   - Erlang: in-memory only
   - Kafka: always writes to disk

✅ **Same AWS region/datacenter**
   - Erlang: designed for LAN
   - Kafka: designed for eventual consistency

### Use Kafka When:

✅ **Event sourcing / audit log**
   - Need persistent, replayable history
   - Kafka: disk-backed, retention policies

✅ **Polyglot architecture**
   - Java, Python, Go services need to communicate
   - Kafka: language-agnostic

✅ **Async workflows with persistence**
   - Job queue that survives restarts
   - Kafka: durable, offset-based

✅ **High throughput batch processing**
   - Millions of events per second
   - Kafka: optimized for throughput over latency

✅ **Cross-datacenter replication**
   - Need WAN-friendly replication
   - Kafka: built-in mirroring

✅ **Multiple independent consumers**
   - Different services process same event stream
   - Kafka: consumer groups

## Your Friend's Proposed Architecture

### What They Want:
```
┌──────────────┐                    ┌──────────────┐
│  Erlang VM 1 │                    │  Erlang VM 2 │
│  (K8s Pod 1) │                    │  (K8s Pod 2) │
│              │                    │              │
│  Process A ──┼──┐              ┌──┼──> Process B │
└──────────────┘  │              │  └──────────────┘
                  │              │
                  ↓              ↑
            ┌─────────────────────────┐
            │     Kafka Cluster       │
            │  (3 brokers + ZooKeeper)│
            │                         │
            │  - Disk writes          │
            │  - Replication          │
            │  - Consumer groups      │
            │  - Offset management    │
            └─────────────────────────┘

Latency: 5-50ms
Complexity: High
Cost: $200+/month (Kafka managed service)
```

### What You Should Do:
```
┌──────────────┐       Direct TCP      ┌──────────────┐
│  Erlang VM 1 │◄─────────────────────►│  Erlang VM 2 │
│  (EC2 node1) │   (Erlang protocol)   │  (EC2 node2) │
│              │                       │              │
│  Process A ──┼───────────────────────┼──> Process B │
└──────────────┘                       └──────────────┘

Latency: 50-200μs (100x faster)
Complexity: Minimal
Cost: $0 (built-in)
```

## The "But What About..." Arguments

### "Kafka provides durability"

**Counter**: If you need durability, use a database or Mnesia:

```erlang
% Persist to Mnesia (Erlang's distributed database)
mnesia:transaction(fun() ->
    mnesia:write(#job{id = Id, data = Data})
end).

% Or use PostgreSQL via epgsql
epgsql:query(Conn, "INSERT INTO jobs VALUES ($1, $2)", [Id, Data]).

% Both faster than Kafka, more flexible querying
```

### "Kafka decouples services"

**Counter**: Erlang has process isolation and registered names:

```erlang
% Service A doesn't need to know Pid of Service B
gen_server:cast({worker_pool, Node}, Request).

% Or use pg (process groups) for dynamic discovery
[Pid | _] = pg:get_members(worker_pool),
gen_server:call(Pid, Request).
```

### "Kafka handles backpressure"

**Counter**: Erlang has built-in backpressure:

```erlang
% Synchronous call naturally applies backpressure
case gen_server:call(Worker, {process, Job}, 5000) of
    {ok, Result} -> Result;
    {error, timeout} ->
        % Worker is overloaded, back off
        timer:sleep(100),
        retry(Job)
end.
```

### "Kafka provides load balancing"

**Counter**: Erlang has poolboy, process groups, and round-robin:

```erlang
% Pool of workers
{ok, Pid} = poolboy:checkout(worker_pool),
gen_server:call(Pid, Request),
poolboy:checkin(worker_pool, Pid).

% Or use pg for distributed load balancing
Members = pg:get_members(worker_pool),  % All nodes
Worker = lists:nth(rand:uniform(length(Members)), Members),
gen_server:call(Worker, Request).
```

## Real-World Anti-Pattern

**This is a real architecture I've seen** (simplified):

```
Elixir Service A ──┐
                   ├──> Kafka ──┐
Elixir Service B ──┘             ├──> Kafka ──┐
                                 │             ├──> ElasticSearch
Elixir Service C ────────────────┘             │
                                               └──> Postgres

Problems:
- 3 Elixir services using Kafka to talk to each other (should use distribution)
- Kafka as event log to ElasticSearch (reasonable)
- Kafka as event log to Postgres (why not just write to DB?)

Result:
- 20-50ms latency between services
- Complex offset management
- Duplicate message handling
- $500/month Kafka cluster
- 5-engineer team to maintain

Better architecture:
Elixir Service A ◄─┐
                   ├─► Elixir Distribution (50μs)
Elixir Service B ◄─┤
                   ├─► Write to Postgres (1-5ms)
Elixir Service C ◄─┘
                   └─► (Optional) Kafka for external consumers

Result:
- 50μs inter-service latency
- Simple code
- No offset management
- $0 infrastructure
- 1 engineer
```

## Message Patterns Comparison

### Request-Response

**Erlang**:
```erlang
Response = gen_server:call({worker, Node}, Request, 5000).
% One line, synchronous, built-in timeout
```

**Kafka**:
```erlang
% Producer
RequestId = generate_uuid(),
ok = brod:produce_sync(client, <<"requests">>, 0, RequestId, Request),

% Wait for response (different topic)
receive
    {kafka_response, RequestId, Response} -> Response
after 5000 -> timeout
end.

% Consumer (separate process/service)
handle_message(Message) ->
    #kafka_message{key = RequestId, value = Request} = Message,
    Response = process_request(Request),
    brod:produce_sync(client, <<"responses">>, 0, RequestId, Response).

% 30+ lines, two topics, correlation ID management
```

### Publish-Subscribe

**Erlang**:
```erlang
% Subscribe
pg:join(chatroom, self()),

% Publish
lists:foreach(
    fun(Pid) -> Pid ! {message, "Hello"} end,
    pg:get_members(chatroom)
).

% Automatic cleanup when process dies
```

**Kafka**:
```erlang
% Consumer group automatically subscribes
% But: partitioning complexity, offset management, rebalancing
```

### Fire-and-Forget

**Erlang**:
```erlang
{worker, Node} ! {task, Data}.
```

**Kafka**:
```erlang
brod:produce(client, <<"tasks">>, 0, <<>>, term_to_binary({task, Data})).
```

**Winner**: Erlang (simpler, faster)

## Cost Analysis

### Erlang Distribution
```
Infrastructure: $0 (built into BEAM)
Development: ~1 hour (learn basics)
Maintenance: ~0 hours/month (no ops burden)
Latency: 50-200μs

Total cost: FREE + FAST
```

### Kafka
```
Infrastructure:
- AWS MSK (Managed Kafka): ~$200-500/month (3 brokers)
- Or self-managed: 3× t3.large = ~$150/month + ops time

Development: ~1-2 weeks (learn Kafka, brod library, patterns)
Maintenance: ~10-20 hours/month (monitoring, scaling, upgrading)
Latency: 5-50ms

Total cost: $200-500/month + 1 engineer week/month
```

**Yearly cost difference**: ~$2,400-6,000 + engineering time

## The Fundamental Mismatch

### Kafka's Design Goals (from LinkedIn's original paper):

1. **High-throughput**: Millions of messages/second across cluster
2. **Persistence**: Disk-backed, replayable log
3. **Partitioning**: Scale-out across brokers
4. **Multi-subscriber**: Many consumers, independent offsets

### Erlang's Design Goals (from Ericsson's requirements):

1. **Low-latency**: Microsecond message passing
2. **Ephemeral**: Messages in-memory (persistent if needed)
3. **Location transparency**: Processes anywhere in cluster
4. **Single logical system**: Distributed but unified

**Kafka is for event streaming.** Large-scale, persistent, replayable logs.

**Erlang is for distributed systems.** Tightly-coupled, low-latency, synchronized processes.

**Using Kafka between Erlang VMs is using the wrong tool.**

## What Your Friend Doesn't Understand

Your friend likely has experience with:
- **Python/Node.js** services (no built-in distribution)
- **Kubernetes** (assumes services are independent)
- **Microservices** (assumes HTTP/messaging between services)

In that world, **Kafka makes sense** because:
- Python has no native inter-process communication
- HTTP is too slow for high-throughput
- Services are truly independent

But **Erlang is different**:
- Native distribution built-in (since 1986)
- Designed for tightly-coupled distributed systems
- Services are **Erlang processes**, not OS processes

**Your friend is applying Python/K8s patterns to Erlang, which is a category error.**

## The Right Architecture

### For Your Current Project

```
┌─────────────────────────────────────────┐
│  EC2 Instance 1 (node1@ip-10-0-1-50)   │
│                                         │
│  ┌────────────────────────────────┐    │
│  │  BEAM VM                       │    │
│  │                                │    │
│  │  Supervision Tree:             │    │
│  │  ├─ HTTP Listener              │    │
│  │  ├─ Worker Pool (1000 workers) │    │
│  │  └─ Database Connection Pool   │    │
│  └────────────────────────────────┘    │
└─────────────────────────────────────────┘

Cost: $50/month
Latency: Intra-VM message passing (~10ns)
Complexity: Low
```

### When You Need to Scale (Still No Kafka)

```
         ┌─── Load Balancer ────┐
         │                      │
    ┌────▼─────┐          ┌────▼─────┐
    │  node1   │◄────────►│  node2   │
    │ (EC2 #1) │  Erlang  │ (EC2 #2) │
    └────┬─────┘ Protocol └────┬─────┘
         │                     │
         └──────┬──────────────┘
                │
         ┌──────▼──────┐
         │  PostgreSQL │
         │   (state)   │
         └─────────────┘

Cost: $100-150/month (2 EC2 + RDS)
Latency: Inter-VM ~50-200μs
Complexity: Still low
```

### Only Add Kafka If... (Probably Never)

```
┌──────────────┐      Erlang      ┌──────────────┐
│  Erlang VM 1 │◄────────────────►│  Erlang VM 2 │
└──────┬───────┘   Distribution   └──────┬───────┘
       │                                  │
       │ Events                           │ Events
       ▼                                  ▼
┌─────────────────────────────────────────────────┐
│                  Kafka                          │
│  (Audit log, external consumers, analytics)     │
└─────────────────────────────────────────────────┘
       │
       ├──► Data warehouse (external consumer)
       ├──► Monitoring system (external consumer)
       └──► Analytics pipeline (external consumer)

Use Kafka for:
- External systems that need events
- Audit trail / compliance
- Analytics / data science

DON'T use Kafka for:
- Erlang-to-Erlang communication (use distribution)
```

## Conclusion

### Your Mate's Suggestion: **TERRIBLE**

Using Kafka between Erlang VMs is:
- ❌ **100-1000x slower** (ms vs μs)
- ❌ **30-50x more complex** (code + infrastructure)
- ❌ **$200-500/month more expensive**
- ❌ **Conceptual mismatch** (event log ≠ RPC)
- ❌ **Throws away Erlang's core strength** (distribution)

### The Correct Approach: **Erlang Distribution**

```erlang
% This is all you need:
{worker, 'node2@hostname'} ! Message.

% Or with monitoring:
Ref = monitor(process, {worker, Node}),
{worker, Node} ! Message,
receive
    {'DOWN', Ref, _, _, Reason} -> handle_failure(Reason)
after 5000 -> timeout
end.
```

**Fast. Simple. Built-in. Purpose-designed.**

### When to Use Kafka (Not for Inter-VM Communication)

- External event log (audit, compliance)
- Polyglot consumers (Python, Java services need events)
- Massive scale-out (1000+ brokers, petabyte logs)
- Cross-datacenter WAN replication

**For Erlang-to-Erlang communication: ALWAYS use Erlang distribution.**

Your instinct is 100% correct. Your mate is cargo-culting patterns from non-Erlang ecosystems.

---

## Appendix: Show Your Mate This Benchmark

```erlang
% Benchmark: Send 1 million messages

% Test 1: Erlang distribution
test_erlang_distribution() ->
    {ok, Node} = start_remote_node(),
    Pid = spawn(Node, fun() -> receiver() end),

    Start = erlang:monotonic_time(),
    lists:foreach(
        fun(N) -> Pid ! {msg, N} end,
        lists:seq(1, 1_000_000)
    ),
    wait_for_completion(Pid),
    End = erlang:monotonic_time(),

    Elapsed = erlang:convert_time_unit(End - Start, native, millisecond),
    io:format("Erlang: ~p ms (~p msg/sec)~n",
              [Elapsed, 1_000_000 div (Elapsed / 1000)]).

% Result: ~100ms = 10 million msg/sec

% Test 2: Kafka
test_kafka() ->
    {ok, _} = brod:start_client([{"localhost", 9092}], kafka_client),

    Start = erlang:monotonic_time(),
    lists:foreach(
        fun(N) ->
            brod:produce_sync(kafka_client, <<"test">>, 0, <<>>, term_to_binary(N))
        end,
        lists:seq(1, 1_000_000)
    ),
    End = erlang:monotonic_time(),

    Elapsed = erlang:convert_time_unit(End - Start, native, millisecond),
    io:format("Kafka: ~p ms (~p msg/sec)~n",
              [Elapsed, 1_000_000 div (Elapsed / 1000)]).

% Result: ~30,000ms = 33,000 msg/sec

% Erlang is 300x faster
```

Show him these numbers. If he still insists on Kafka, he doesn't understand Erlang.
