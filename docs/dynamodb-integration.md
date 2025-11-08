# DynamoDB Integration Strategy

## Overview

This document outlines the strategy for integrating DynamoDB with the hello-erlang application, supporting both production EC2 deployments and local development.

**Key Principles:**
- Localhost development uses real AWS DynamoDB resources (no local emulation)
- Separate stacks for infrastructure vs. shared resources (DynamoDB tables)
- Environment isolation: dev, staging, prod each get their own tables
- Non-invasive: DynamoDB configuration via `sys.config`, minimal code changes

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Project Organization                                        │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Compute Stack (per environment)                            │
│  ├─ EC2 Instance + ALB                                      │
│  ├─ Security Groups                                         │
│  └─ IAM Role (with DynamoDB permissions)                    │
│                                                              │
│  Data Stack (per environment)                               │
│  ├─ DynamoDB Tables                                         │
│  ├─ Indexes                                                 │
│  └─ Table exports/ARNs                                      │
│                                                              │
│  Localhost Development                                      │
│  └─ Uses Data Stack (dev) via AWS credentials               │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## 1. AWS SDK Library Choice

### Recommended: aws-beam/aws-erlang

**Repository:** https://github.com/aws-beam/aws-erlang

**Advantages:**
- Auto-generated from official AWS service descriptions (same as official SDKs)
- Comprehensive coverage of all AWS services including DynamoDB
- Actively maintained
- Similar to boto3 in approach (generated from service specs)
- Type specs included for better dialyzer support

**Add to rebar.config:**

```erlang
{deps, [
    {cowboy, "2.12.0"},
    {aws, {git, "https://github.com/aws-beam/aws-erlang.git", {branch, "main"}}}
]}.
```

### Alternative: erlcloud

**Repository:** https://github.com/erlcloud/erlcloud

**Advantages:**
- Mature, battle-tested library
- Hand-written (not generated)
- Good documentation and examples
- Supports DynamoDB, S3, EC2, SQS, etc.

**Add to rebar.config:**

```erlang
{deps, [
    {cowboy, "2.12.0"},
    {erlcloud, "3.7.4"}
]}.
```

**Recommendation:** Use `aws-beam/aws-erlang` as it's the closest equivalent to boto3's approach.

## 2. Project Structure: Two-Stack Strategy

### Why Two Stacks?

1. **Compute Stack** (existing `stack.yaml`):
   - Short-lived resources (EC2, ALB)
   - Frequently created/destroyed during development
   - Can be torn down without data loss

2. **Data Stack** (new `dynamodb-stack.yaml`):
   - Persistent resources (DynamoDB tables)
   - Created once per environment
   - Protected from accidental deletion
   - Shared between deployments

### Directory Structure

```
config/
├── stack.yaml              # Compute resources (existing)
├── dynamodb-stack.yaml     # Data resources (new)
└── aws.sh                  # Shared configuration

scripts/
├── aws-stack.sh            # Generic stack management (existing)
├── aws-deploy.sh           # App deployment (existing)
├── data-stack.sh           # Data stack wrapper (new)
└── compute-stack.sh        # Compute stack wrapper (new)
```

## 3. DynamoDB Stack Template

### config/dynamodb-stack.yaml

```yaml
AWSTemplateFormatVersion: '2010-09-09'
Description: DynamoDB tables for hello-erlang application

Parameters:
  Environment:
    Type: String
    AllowedValues:
      - dev
      - staging
      - prod
    Description: Environment name

  BillingMode:
    Type: String
    Default: PAY_PER_REQUEST
    AllowedValues:
      - PROVISIONED
      - PAY_PER_REQUEST
    Description: DynamoDB billing mode (on-demand recommended for dev/staging)

  DeletionProtection:
    Type: String
    Default: 'false'
    AllowedValues:
      - 'true'
      - 'false'
    Description: Enable deletion protection (recommended for prod)

Resources:
  # Example table: Messages
  MessagesTable:
    Type: AWS::DynamoDB::Table
    DeletionPolicy: Retain
    UpdateReplacePolicy: Retain
    Properties:
      TableName: !Sub ${Environment}-hello-erlang-messages
      BillingMode: !Ref BillingMode
      DeletionProtectionEnabled: !Ref DeletionProtection
      AttributeDefinitions:
        - AttributeName: id
          AttributeType: S
        - AttributeName: timestamp
          AttributeType: N
        - AttributeName: user_id
          AttributeType: S
      KeySchema:
        - AttributeName: id
          KeyType: HASH
      GlobalSecondaryIndexes:
        - IndexName: user-timestamp-index
          KeySchema:
            - AttributeName: user_id
              KeyType: HASH
            - AttributeName: timestamp
              KeyType: RANGE
          Projection:
            ProjectionType: ALL
      StreamSpecification:
        StreamViewType: NEW_AND_OLD_IMAGES
      PointInTimeRecoverySpecification:
        PointInTimeRecoveryEnabled: !If [IsProd, true, false]
      Tags:
        - Key: Environment
          Value: !Ref Environment
        - Key: Application
          Value: hello-erlang
        - Key: ManagedBy
          Value: CloudFormation

  # Example table: Sessions
  SessionsTable:
    Type: AWS::DynamoDB::Table
    DeletionPolicy: Retain
    UpdateReplacePolicy: Retain
    Properties:
      TableName: !Sub ${Environment}-hello-erlang-sessions
      BillingMode: !Ref BillingMode
      DeletionProtectionEnabled: !Ref DeletionProtection
      AttributeDefinitions:
        - AttributeName: session_id
          AttributeType: S
      KeySchema:
        - AttributeName: session_id
          KeyType: HASH
      TimeToLiveSpecification:
        Enabled: true
        AttributeName: ttl
      Tags:
        - Key: Environment
          Value: !Ref Environment
        - Key: Application
          Value: hello-erlang
        - Key: ManagedBy
          Value: CloudFormation

Conditions:
  IsProd: !Equals [!Ref Environment, prod]

Outputs:
  MessagesTableName:
    Description: Messages table name
    Value: !Ref MessagesTable
    Export:
      Name: !Sub ${AWS::StackName}-MessagesTableName

  MessagesTableArn:
    Description: Messages table ARN
    Value: !GetAtt MessagesTable.Arn
    Export:
      Name: !Sub ${AWS::StackName}-MessagesTableArn

  SessionsTableName:
    Description: Sessions table name
    Value: !Ref SessionsTable
    Export:
      Name: !Sub ${AWS::StackName}-SessionsTableName

  SessionsTableArn:
    Description: Sessions table ARN
    Value: !GetAtt SessionsTable.Arn
    Export:
      Name: !Sub ${AWS::StackName}-SessionsTableArn

  StackName:
    Description: Stack name for reference
    Value: !Ref AWS::StackName
```

## 4. Updated EC2 Stack with DynamoDB IAM Permissions

### Update config/stack.yaml

Add DynamoDB permissions to the EC2 IAM role:

```yaml
Resources:
  # ... existing resources ...

  EC2Role:
    Type: AWS::IAM::Role
    Properties:
      AssumeRolePolicyDocument:
        Version: '2012-10-17'
        Statement:
          - Effect: Allow
            Principal:
              Service: ec2.amazonaws.com
            Action: sts:AssumeRole
      ManagedPolicyArns:
        - arn:aws:iam::aws:policy/CloudWatchAgentServerPolicy
        - arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore
      Policies:
        - PolicyName: DynamoDBAccess
          PolicyDocument:
            Version: '2012-10-17'
            Statement:
              - Effect: Allow
                Action:
                  - dynamodb:PutItem
                  - dynamodb:GetItem
                  - dynamodb:UpdateItem
                  - dynamodb:DeleteItem
                  - dynamodb:Query
                  - dynamodb:Scan
                  - dynamodb:BatchGetItem
                  - dynamodb:BatchWriteItem
                  - dynamodb:DescribeTable
                Resource:
                  - !Sub
                    - 'arn:aws:dynamodb:${AWS::Region}:${AWS::AccountId}:table/${Environment}-hello-erlang-*'
                    - Environment: !Ref Environment
                  - !Sub
                    - 'arn:aws:dynamodb:${AWS::Region}:${AWS::AccountId}:table/${Environment}-hello-erlang-*/index/*'
                    - Environment: !Ref Environment

  # ... rest of resources ...
```

## 5. Stack Management Scripts

### scripts/data-stack.sh

```bash
#!/bin/bash
# Wrapper for DynamoDB stack management

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

TEMPLATE_FILE="$PROJECT_ROOT/config/dynamodb-stack.yaml"
STACK_PREFIX="${STACK_PREFIX:-hello-erlang-data}"

# Load defaults from aws.sh if it exists
if [ -f "$PROJECT_ROOT/config/aws.sh" ]; then
    source "$PROJECT_ROOT/config/aws.sh"
fi

# Forward to generic stack script with data-specific template
exec "$SCRIPT_DIR/aws-stack.sh" \
    --stack-prefix "$STACK_PREFIX" \
    --template "$TEMPLATE_FILE" \
    "$@"
```

### scripts/compute-stack.sh

```bash
#!/bin/bash
# Wrapper for EC2 compute stack management

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

TEMPLATE_FILE="$PROJECT_ROOT/config/stack.yaml"
STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"

# Load defaults from aws.sh if it exists
if [ -f "$PROJECT_ROOT/config/aws.sh" ]; then
    source "$PROJECT_ROOT/config/aws.sh"
fi

# Forward to generic stack script with compute-specific template
exec "$SCRIPT_DIR/aws-stack.sh" \
    --stack-prefix "$STACK_PREFIX" \
    --template "$TEMPLATE_FILE" \
    "$@"
```

### Make both executable

```bash
chmod +x scripts/data-stack.sh scripts/compute-stack.sh
```

## 6. Erlang Configuration

### Update config/sys.config

```erlang
[
  {kernel, [
    {logger_level, info},
    {logger, [
      {handler, default, logger_std_h, #{level => info}},
      {handler, file_handler, logger_std_h, #{
        level => error,
        config => #{
          file => "/var/log/hello_erlang/errors.log",
          max_no_bytes => 10485760,
          max_no_files => 5
        }
      }}
    ]}
  ]},

  {aws, [
    % AWS SDK configuration
    % On EC2, uses IAM role automatically
    % Locally, uses AWS_PROFILE from environment
    {region, "us-east-1"}  % Override with environment variable
  ]},

  {hello_erlang, [
    % DynamoDB table names - set via environment variables
    % Format: {table_key, TableNameFromCloudFormation}
    {messages_table, "dev-hello-erlang-messages"},
    {sessions_table, "dev-hello-erlang-sessions"}
  ]}
].
```

### Environment-Specific Configuration

Create separate config files per environment:

```
config/
├── sys.config.dev
├── sys.config.staging
├── sys.config.prod
└── sys.config -> sys.config.dev  # Symlink for local dev
```

**Or** use environment variables (recommended):

```erlang
% In hello_erlang_app.erl start/2
get_table_name(Key) ->
    % First try environment variable
    case os:getenv(string:uppercase(atom_to_list(Key))) of
        false ->
            % Fall back to sys.config
            {ok, Name} = application:get_env(hello_erlang, Key),
            Name;
        EnvValue ->
            EnvValue
    end.
```

## 7. Deployment Workflow

### Initial Setup (One-Time Per Environment)

```bash
# 1. Create data stack (persistent)
./scripts/data-stack.sh create dev

# Wait for completion (~2 minutes)
./scripts/data-stack.sh status dev

# Get table names
./scripts/data-stack.sh outputs dev

# 2. Create compute stack (EC2 + ALB)
./scripts/compute-stack.sh create dev --key-name my-key --subnets subnet-x,subnet-y

# 3. Deploy application
./scripts/aws-deploy.sh dev
```

### Regular Application Updates

```bash
# Just deploy application (data stack untouched)
./scripts/aws-deploy.sh dev
```

### Teardown (Development)

```bash
# Delete compute stack (destroys EC2, preserves data)
./scripts/compute-stack.sh delete dev

# Data stack remains intact with all data
# Only delete if you want to remove all data:
# ./scripts/data-stack.sh delete dev  # CAUTION: Data loss!
```

## 8. Local Development Setup

### Prerequisites

1. **AWS Credentials** - Configure AWS CLI:
   ```bash
   aws configure --profile hello-erlang-dev
   # Enter: Access Key ID, Secret Access Key, Region
   ```

2. **Create Dev Data Stack** (if not exists):
   ```bash
   ./scripts/data-stack.sh create dev
   ```

3. **Get Table Names**:
   ```bash
   ./scripts/data-stack.sh outputs dev
   # Note the MessagesTableName and SessionsTableName
   ```

### Local Configuration

Create `.env` file (gitignored):

```bash
# .env
export AWS_PROFILE=hello-erlang-dev
export AWS_REGION=us-east-1
export MESSAGES_TABLE=dev-hello-erlang-messages
export SESSIONS_TABLE=dev-hello-erlang-sessions
```

Update `.gitignore`:

```
.env
```

### Run Locally

```bash
# Load environment
source .env

# Start application
make start

# Or with environment inline
AWS_PROFILE=hello-erlang-dev make start
```

### Update scripts/dev.sh

Add environment variable support:

```bash
#!/bin/bash
# Load .env if it exists
if [ -f .env ]; then
    source .env
fi

# Rest of script...
```

## 9. DynamoDB Client Module

### apps/hello_erlang/src/hello_erlang_db.erl

```erlang
-module(hello_erlang_db).
-export([
    init/0,
    put_message/2,
    get_message/1,
    query_messages_by_user/1
]).

%% Initialize DynamoDB client
init() ->
    % AWS SDK will automatically use:
    % - IAM role credentials on EC2
    % - AWS_PROFILE credentials locally
    {ok, Region} = application:get_env(aws, region),
    aws_client:make_client(Region, <<"dynamodb">>).

%% Put a message
put_message(MessageId, Data) ->
    Client = init(),
    {ok, TableName} = application:get_env(hello_erlang, messages_table),

    Item = #{
        <<"id">> => #{<<"S">> => MessageId},
        <<"timestamp">> => #{<<"N">> => integer_to_binary(erlang:system_time(second))},
        <<"user_id">> => #{<<"S">> => maps:get(<<"user_id">>, Data)},
        <<"content">> => #{<<"S">> => maps:get(<<"content">>, Data)}
    },

    Input = #{
        <<"TableName">> => list_to_binary(TableName),
        <<"Item">> => Item
    },

    case aws_dynamodb:put_item(Client, Input) of
        {ok, _Result, _Response} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Get a message
get_message(MessageId) ->
    Client = init(),
    {ok, TableName} = application:get_env(hello_erlang, messages_table),

    Key = #{
        <<"id">> => #{<<"S">> => MessageId}
    },

    Input = #{
        <<"TableName">> => list_to_binary(TableName),
        <<"Key">> => Key
    },

    case aws_dynamodb:get_item(Client, Input) of
        {ok, #{<<"Item">> := Item}, _Response} -> {ok, Item};
        {ok, #{}, _Response} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% Query messages by user
query_messages_by_user(UserId) ->
    Client = init(),
    {ok, TableName} = application:get_env(hello_erlang, messages_table),

    Input = #{
        <<"TableName">> => list_to_binary(TableName),
        <<"IndexName">> => <<"user-timestamp-index">>,
        <<"KeyConditionExpression">> => <<"user_id = :uid">>,
        <<"ExpressionAttributeValues">> => #{
            <<":uid">> => #{<<"S">> => UserId}
        },
        <<"ScanIndexForward">> => false,  % Newest first
        <<"Limit">> => 100
    },

    case aws_dynamodb:query(Client, Input) of
        {ok, #{<<"Items">> := Items}, _Response} -> {ok, Items};
        {error, Reason} -> {error, Reason}
    end.
```

## 10. Updated Handler Example

### apps/hello_erlang/src/echo_handler.erl

```erlang
-module(echo_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:match_qs([{message, [], <<>>}, {user_id, [], <<"anonymous">>}], Req0) of
        #{message := Message, user_id := UserId} when Message =/= <<>> ->
            % Store message in DynamoDB
            MessageId = generate_id(),
            Data = #{
                <<"user_id">> => UserId,
                <<"content">> => Message
            },

            case hello_erlang_db:put_message(MessageId, Data) of
                ok ->
                    Response = #{
                        <<"id">> => MessageId,
                        <<"message">> => Message,
                        <<"user_id">> => UserId,
                        <<"status">> => <<"stored">>
                    },
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(Response),
                        Req0),
                    {ok, Req, State};
                {error, Reason} ->
                    logger:error("DynamoDB error: ~p", [Reason]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Database error">>,
                        Req0),
                    {ok, Req, State}
            end;
        #{message := <<>>} ->
            % Original echo behavior (no storage)
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                <<>>,
                Req0),
            {ok, Req, State}
    end.

generate_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    base64:encode(<<Timestamp:64, Random:32>>).
```

## 11. Testing

### Local Testing

```bash
# Source environment
source .env

# Start app
make start

# Test message storage
curl "http://localhost:8080/echo?message=hello&user_id=user123"

# Verify in DynamoDB
aws dynamodb scan \
    --table-name dev-hello-erlang-messages \
    --profile hello-erlang-dev
```

### EC2 Testing

```bash
# Deploy to dev
./scripts/aws-deploy.sh dev

# Test via ALB
curl "http://$(./scripts/compute-stack.sh outputs dev | grep LoadBalancerDNS | awk '{print $2}')/echo?message=production-test&user_id=testuser"

# Check CloudWatch Logs for errors
aws logs tail /aws/ec2/hello-erlang/dev --follow
```

## 12. Cost Considerations

### Development Environment

- **DynamoDB**: PAY_PER_REQUEST (on-demand)
  - First 25 GB storage: Free
  - Free tier: 25 WCU + 25 RCU per month
  - Cost: ~$1-5/month for light development

- **EC2**: Can be stopped when not in use
  - Keep data stack running (minimal cost)
  - Delete compute stack to save on EC2/ALB costs

### Production Environment

- **DynamoDB**: Consider PROVISIONED for predictable workloads
  - Enable Point-in-Time Recovery
  - Enable deletion protection
  - Auto-scaling for capacity

## 13. Migration Path

### Phase 1: Add DynamoDB Support (Non-Breaking)
1. Add aws-beam dependency to rebar.config
2. Create dynamodb-stack.yaml
3. Create data-stack.sh wrapper
4. Deploy data stack to dev
5. Add hello_erlang_db module
6. Update sys.config with table names

### Phase 2: Update Handlers (Backward Compatible)
1. Update echo_handler to store messages
2. Keep existing echo-only behavior
3. Test locally with .env
4. Deploy to dev environment

### Phase 3: Production Rollout
1. Create staging data stack
2. Deploy and test on staging
3. Create prod data stack with protection enabled
4. Deploy to production
5. Monitor CloudWatch for DynamoDB errors

## 14. Best Practices

1. **Never commit AWS credentials** - Use IAM roles and profiles
2. **Use separate AWS accounts** for dev/staging/prod (optional but recommended)
3. **Enable DynamoDB deletion protection** for production
4. **Use CloudFormation exports** for cross-stack references
5. **Tag all resources** for cost tracking
6. **Monitor DynamoDB metrics** - throttling, latency, consumed capacity
7. **Implement exponential backoff** for DynamoDB retries
8. **Use batch operations** when possible to reduce costs
9. **Set up CloudWatch alarms** for error rates and throttling

## 15. Common Commands Reference

```bash
# Data Stack Management
./scripts/data-stack.sh create dev
./scripts/data-stack.sh status dev
./scripts/data-stack.sh outputs dev
./scripts/data-stack.sh delete dev  # CAUTION: Data loss!

# Compute Stack Management
./scripts/compute-stack.sh create dev --key-name my-key --subnets subnet-x,subnet-y
./scripts/compute-stack.sh update dev
./scripts/compute-stack.sh delete dev

# Application Deployment
./scripts/aws-deploy.sh dev

# Local Development
source .env
make start
make stop

# DynamoDB CLI
aws dynamodb list-tables --profile hello-erlang-dev
aws dynamodb describe-table --table-name dev-hello-erlang-messages
aws dynamodb scan --table-name dev-hello-erlang-messages --max-items 10
```

## References

- [aws-beam/aws-erlang GitHub](https://github.com/aws-beam/aws-erlang)
- [DynamoDB Best Practices](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/best-practices.html)
- [DynamoDB Developer Guide](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/)
- [Erlang Application Configuration](https://www.erlang.org/doc/design_principles/applications.html)
