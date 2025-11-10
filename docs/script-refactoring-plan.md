# AWS Scripts Refactoring Plan

## Goal

Refactor `scripts/aws/` from monolithic multi-command scripts to single-purpose scripts following the `verb-noun.sh` naming pattern.

## Current State (Problems)

```
scripts/aws/
├── stack.sh                 # 8 commands (deploy, delete, update, status, outputs, resources, events, restart-agents)
├── artifacts.sh             # 5 commands (create, delete, empty, list, upload)
├── deploy-handlers.sh       # Redundant - should be part of deploy
├── test.sh                  # 2 commands (echo, add)
└── debug.sh                 # 7 commands (list-builds, build-logs, list-artifacts, list-deployments, deploy-logs, instance-logs, list-stacks)
```

**Issues:**
- ❌ Unclear what commands are available without reading each script
- ❌ `deploy-handlers.sh` duplicates logic that should be in deploy workflow
- ❌ Multi-command scripts require parsing subcommands
- ❌ Hard to compose scripts or use in CI/CD
- ❌ Inconsistent naming (some use dashes, some don't)

## Target State

```
scripts/aws/
# Stack Operations (8 scripts)
├── deploy-stack.sh          # Complete deploy: bucket check → upload handlers → upload template → create stack
├── update-stack.sh          # Complete update: upload handlers → upload template → update stack
├── delete-stack.sh          # Delete stack with optional bucket cleanup
├── show-stack.sh            # Show stack status
├── show-outputs.sh          # Show stack outputs
├── show-resources.sh        # Show stack resources
├── show-events.sh           # Show stack events
├── restart-agents.sh        # Restart CodeDeploy/CloudWatch agents

# Bucket Operations (4 scripts)
├── create-bucket.sh         # Create stack-artifacts bucket (prerequisite for deploy)
├── delete-bucket.sh         # Delete bucket
├── empty-bucket.sh          # Empty bucket contents
├── list-bucket.sh           # List bucket contents with versions

# Build Operations (2 scripts)
├── list-builds.sh           # List CodeBuild builds
├── show-build-logs.sh       # Show logs for a specific build

# Deployment Operations (2 scripts)
├── list-deployments.sh      # List CodeDeploy deployments
├── show-deployment-logs.sh  # Show logs for a specific deployment

# Instance Operations (1 script)
├── show-instance-logs.sh    # Show EC2 UserData logs via SSM

# Stack Discovery (1 script)
├── list-stacks.sh           # List all CloudFormation stacks

# Testing Operations (2 scripts)
├── test-echo.sh             # Test echo endpoint
├── test-add.sh              # Test add endpoint

Total: 20 scripts (removed upload-artifact, integrated deploy-handlers)
```

**Benefits:**
- ✅ Clear visibility: `ls scripts/aws/` shows all available commands
- ✅ Single responsibility: each script does one thing
- ✅ Consistent naming: `verb-noun.sh`
- ✅ Easy composition: scripts can call each other
- ✅ Simple usage: `./scripts/aws/deploy-stack.sh dev`
- ✅ CI/CD friendly: no subcommand parsing needed

## Implementation Phases

### Phase 1: Core Deployment Workflow (Priority: HIGH)

**Goal:** Make deployment simple and integrated

**Tasks:**
1. Create `deploy-stack.sh`
   - Check/create bucket if needed (or error with helpful message)
   - Package and upload Lambda handlers
   - Upload CloudFormation template
   - Create stack with all parameters
   - Wait for completion
   - Show outputs

2. Create `update-stack.sh`
   - Package and upload Lambda handlers
   - Upload CloudFormation template
   - Update stack
   - Wait for completion
   - Show outputs

3. Create `delete-stack.sh`
   - Delete stack
   - Optionally empty and delete buckets (with confirmation)

4. Remove `deploy-handlers.sh` (logic integrated into deploy/update)

**Test:** Deploy a stack end-to-end with single command

**Files Modified:**
- New: `scripts/aws/deploy-stack.sh`
- New: `scripts/aws/update-stack.sh`
- New: `scripts/aws/delete-stack.sh`
- Delete: `scripts/aws/deploy-handlers.sh`
- Keep: `scripts/aws/stack.sh` (temporarily, for other commands)

### Phase 2: Stack Inspection Commands (Priority: MEDIUM)

**Goal:** Break out stack.sh into focused scripts

**Tasks:**
1. Create `show-stack.sh` (from stack.sh status)
2. Create `show-outputs.sh` (from stack.sh outputs)
3. Create `show-resources.sh` (from stack.sh resources)
4. Create `show-events.sh` (from stack.sh events)
5. Create `restart-agents.sh` (from stack.sh restart-agents)
6. Remove `scripts/aws/stack.sh`

**Common Functions:**
- All scripts share: `get_stack_name()` function
- Consider creating `scripts/aws/lib/common.sh` for shared functions

**Test:** Verify all show-* commands work after refactor

**Files Modified:**
- New: `scripts/aws/show-stack.sh`
- New: `scripts/aws/show-outputs.sh`
- New: `scripts/aws/show-resources.sh`
- New: `scripts/aws/show-events.sh`
- New: `scripts/aws/restart-agents.sh`
- Delete: `scripts/aws/stack.sh`

### Phase 3: Bucket Management (Priority: MEDIUM)

**Goal:** Separate bucket operations from artifacts.sh

**Tasks:**
1. Create `create-bucket.sh` (from artifacts.sh create)
2. Create `delete-bucket.sh` (from artifacts.sh delete)
3. Create `empty-bucket.sh` (from artifacts.sh empty)
4. Create `list-bucket.sh` (from artifacts.sh list)
5. Remove `upload` command (internal only)
6. Remove `scripts/aws/artifacts.sh`

**Common Functions:**
- Share: `get_bucket_name()` function
- Consider: `scripts/aws/lib/bucket-common.sh`

**Test:** Verify bucket lifecycle (create → list → empty → delete)

**Files Modified:**
- New: `scripts/aws/create-bucket.sh`
- New: `scripts/aws/delete-bucket.sh`
- New: `scripts/aws/empty-bucket.sh`
- New: `scripts/aws/list-bucket.sh`
- Delete: `scripts/aws/artifacts.sh`

### Phase 4: Testing Commands (Priority: LOW)

**Goal:** Separate test commands

**Tasks:**
1. Create `test-echo.sh` (from test.sh echo)
2. Create `test-add.sh` (from test.sh add)
3. Remove `scripts/aws/test.sh`

**Common Functions:**
- Share: `get_alb_dns()` function
- Consider: `scripts/aws/lib/test-common.sh`

**Test:** Verify both endpoints work

**Files Modified:**
- New: `scripts/aws/test-echo.sh`
- New: `scripts/aws/test-add.sh`
- Delete: `scripts/aws/test.sh`

### Phase 5: Debug/Inspection Commands (Priority: LOW)

**Goal:** Separate debug commands

**Tasks:**
1. Create `list-builds.sh` (from debug.sh list-builds)
2. Create `show-build-logs.sh` (from debug.sh build-logs)
3. Create `list-artifacts.sh` (from debug.sh list-artifacts)
4. Create `list-deployments.sh` (from debug.sh list-deployments)
5. Create `show-deployment-logs.sh` (from debug.sh deploy-logs)
6. Create `show-instance-logs.sh` (from debug.sh instance-logs)
7. Create `list-stacks.sh` (from debug.sh list-stacks)
8. Remove `scripts/aws/debug.sh`

**Common Functions:**
- Share: `get_stack_output()` function
- Consider: `scripts/aws/lib/debug-common.sh`

**Test:** Verify all inspection commands work

**Files Modified:**
- New: `scripts/aws/list-builds.sh`
- New: `scripts/aws/show-build-logs.sh`
- New: `scripts/aws/list-artifacts.sh`
- New: `scripts/aws/list-deployments.sh`
- New: `scripts/aws/show-deployment-logs.sh`
- New: `scripts/aws/show-instance-logs.sh`
- New: `scripts/aws/list-stacks.sh`
- Delete: `scripts/aws/debug.sh`

## Shared Code Strategy

### Option A: Inline Duplication
- Each script is fully self-contained
- Copy common functions into each script
- **Pros:** No dependencies, easy to understand
- **Cons:** Duplication, harder to maintain

### Option B: Shared Library
- Create `scripts/aws/lib/common.sh` with shared functions
- Scripts source the library
- **Pros:** DRY, easier to maintain
- **Cons:** Dependency, slightly more complex

**Recommendation:** Start with Option A (inline), move to Option B if duplication becomes problematic.

## Documentation Updates

After each phase, update:
- `infra/README.md` - Update deployment instructions
- `docs/codestar-connection-setup.md` - Update script paths
- Top-level `README.md` if it exists - Update quick start

## Migration Guide

### Before (Old Way)
```bash
# Three separate commands
./scripts/aws/artifacts.sh create dev
./scripts/aws/deploy-handlers.sh dev
./scripts/aws/stack.sh deploy dev

# Check status
./scripts/aws/stack.sh status dev
./scripts/aws/stack.sh outputs dev

# Debug
./scripts/aws/debug.sh list-builds dev
./scripts/aws/test.sh echo dev "hello"
```

### After (New Way)
```bash
# Single command (Phase 1)
./scripts/aws/create-bucket.sh dev  # First time only
./scripts/aws/deploy-stack.sh dev

# Check status (Phase 2)
./scripts/aws/show-stack.sh dev
./scripts/aws/show-outputs.sh dev

# Debug (Phase 5)
./scripts/aws/list-builds.sh dev
./scripts/aws/test-echo.sh dev "hello"
```

## Rollout Strategy

1. **Create new scripts alongside old** - Don't delete old scripts until new ones are tested
2. **Test each phase** - Deploy to dev environment after each phase
3. **Update docs incrementally** - Update documentation as scripts are added
4. **Mark old scripts deprecated** - Add warning messages to old scripts pointing to new ones
5. **Delete old scripts** - Only after new scripts are proven in production

## Phase 1 Implementation Details

### deploy-stack.sh Structure

```bash
#!/bin/bash
# Deploy CloudFormation stack with all prerequisites

set -e
cd "$(dirname "$0")/../.."

# Source config
[ -f "config/env.sh" ] && source "config/env.sh"

STACK_PREFIX="${STACK_PREFIX:-hello-erlang}"
TEMPLATE_FILE="infra/stack.yaml"

usage() {
    echo "Usage: $0 <environment> [options]"
    echo ""
    echo "Complete stack deployment workflow:"
    echo "  1. Verify stack-artifacts bucket exists"
    echo "  2. Package and upload Lambda handlers"
    echo "  3. Upload CloudFormation template"
    echo "  4. Create stack"
    echo ""
    echo "Environments: dev, staging, prod"
    echo ""
    echo "Options:"
    echo "  --instance-type <type>    - EC2 instance type (default: t3.micro)"
    echo "  --erlang-version <ver>    - Erlang/OTP version (default: 27.1)"
    echo "  ... (other options from current stack.sh)"
    exit 1
}

get_stack_name() { ... }
get_bucket_name() { ... }
check_bucket_exists() { ... }
package_lambda_handlers() { ... }
upload_template() { ... }
create_stack() { ... }

main() {
    ENV=$1
    shift

    echo "=== Stack Deployment ==="
    echo "Environment: $ENV"
    echo ""

    echo "Step 1: Checking prerequisites..."
    check_bucket_exists "$ENV" || {
        echo "Error: Stack-artifacts bucket not found"
        echo "Create it: ./scripts/aws/create-bucket.sh $ENV"
        exit 1
    }

    echo "Step 2: Packaging Lambda handlers..."
    package_lambda_handlers "$ENV"

    echo "Step 3: Uploading template..."
    TEMPLATE_URL=$(upload_template "$ENV")

    echo "Step 4: Creating stack..."
    create_stack "$ENV" "$TEMPLATE_URL" "$@"

    echo ""
    echo "✓ Deployment complete!"
}

main "$@"
```

## Success Criteria

- [ ] Phase 1: Can deploy stack with single command
- [ ] Phase 2: All stack inspection commands work standalone
- [ ] Phase 3: Bucket management is clear and separate
- [ ] Phase 4: Test commands are simple and focused
- [ ] Phase 5: Debug commands are discoverable
- [ ] Documentation updated for all phases
- [ ] Old scripts removed
- [ ] CI/CD (if exists) updated to use new scripts

## Timeline Estimate

- Phase 1: 2-3 hours (critical path)
- Phase 2: 1-2 hours
- Phase 3: 1 hour
- Phase 4: 30 minutes
- Phase 5: 1-2 hours
- Documentation: 1 hour

**Total:** ~6-10 hours of implementation work

## Questions to Resolve

1. **Shared library or inline duplication?**
   - Decision: Start inline, refactor to library if needed

2. **Error handling strategy?**
   - Use `set -e` consistently
   - Provide helpful error messages with next steps

3. **Output verbosity?**
   - Show progress steps by default
   - Consider `--quiet` flag for CI/CD

4. **Testing strategy?**
   - Manual testing per phase
   - Consider adding integration tests later

5. **Backward compatibility?**
   - Keep old scripts with deprecation warnings during transition
   - Remove after 1 week of successful usage
