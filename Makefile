.PHONY: help compile release clean start stop restart status ping package aws-help

help:
	@echo "Hello Erlang - Development Commands"
	@echo ""
	@echo "Build commands:"
	@echo "  make compile  - Compile Erlang code"
	@echo "  make release  - Build release"
	@echo "  make clean    - Remove build artifacts"
	@echo ""
	@echo "Development commands:"
	@echo "  make start    - Start server in daemon mode"
	@echo "  make stop     - Stop server"
	@echo "  make restart  - Restart server"
	@echo "  make status   - Check server status"
	@echo "  make console  - Start with console"
	@echo ""
	@echo "Deployment commands:"
	@echo "  make package  - Build deployment tarball"
	@echo ""
	@echo "AWS commands:"
	@echo "  make aws-help - Show AWS deployment commands"
	@echo ""
	@echo "Or use scripts directly:"
	@echo "  scripts/build.sh {compile|release|clean}"
	@echo "  scripts/dev.sh {start|stop|restart|status|ping}"
	@echo "  scripts/deploy.sh {package}"
	@echo "  scripts/aws/stack.sh {create|delete|list|...}"
	@echo "  scripts/aws/deploy.sh <env>"

aws-help:
	@echo "Hello Erlang - AWS Deployment Commands"
	@echo ""
	@echo "Infrastructure (CloudFormation):"
	@echo "  scripts/aws/stack.sh create <env> --key-name <name>"
	@echo "  scripts/aws/stack.sh delete <env>"
	@echo "  scripts/aws/stack.sh status <env>"
	@echo "  scripts/aws/stack.sh outputs <env>"
	@echo "  scripts/aws/stack.sh events <env>"
	@echo "  scripts/aws/stack.sh list"
	@echo ""
	@echo "Application Deployment:"
	@echo "  scripts/aws/deploy.sh <env>"
	@echo "  scripts/aws/deploy.sh <env> --skip-build"
	@echo "  scripts/aws/deploy.sh <env> --key-file <path>"
	@echo ""
	@echo "Environments: dev, staging, prod"
	@echo ""
	@echo "Example workflow:"
	@echo "  1. Create stack:    scripts/aws/stack.sh create dev --key-name my-key"
	@echo "  2. Deploy app:      scripts/aws/deploy.sh dev"
	@echo "  3. Check outputs:   scripts/aws/stack.sh outputs dev"
	@echo "  4. Update code:     scripts/aws/deploy.sh dev"
	@echo "  5. Delete stack:    scripts/aws/stack.sh delete dev"

# Build targets
compile:
	@scripts/build.sh compile

release:
	@scripts/build.sh release

clean:
	@scripts/build.sh clean

# Dev targets
start:
	@scripts/dev.sh start

stop:
	@scripts/dev.sh stop

restart:
	@scripts/dev.sh restart

status:
	@scripts/dev.sh status

console:
	@scripts/dev.sh console

# Deployment targets
package:
	@scripts/deploy.sh package
