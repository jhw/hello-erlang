### Completed

- ✅ CodeBuild refactor - Build releases in AWS CodeBuild instead of on EC2
- ✅ CodeDeploy integration - Automated deployments via AWS CodeDeploy with lifecycle hooks
- ✅ S3 event-driven deployments - Lambda triggers CodeDeploy on artifact upload
- ✅ Configuration management - Renamed aws.sh to env.sh for multi-service config

### Short Term

- logging >>> chatops (Slack/Discord notifications for deployments)
- state >>> dynamodb (Replace in-memory state with persistent storage)
- telemetry >>> alarms (CloudWatch alarms for health monitoring)
- python >> erlport (Erlang-Python integration)

### Medium Term

- CodePipeline integration - Replace manual build trigger with GitHub webhooks
- Auto Scaling Group - Replace single EC2 with ASG for high availability
- Blue/Green deployments - Zero-downtime deployments via CodeDeploy
- Hot code reloading - Implement relup/appup for live updates
- auth (Authentication/authorization layer)
- websockets (Real-time bidirectional communication)
- HTTPS/TLS (SSL certificate via ACM)

### Long Term

- Multi-region deployment - Active-active across regions
- Erlang clustering - Distributed Erlang nodes
- Observability suite - DataDog/New Relic integration
- CI/CD pipeline - Full GitOps workflow
