.PHONY: help compile release clean start stop restart status ping package

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
	@echo "Or use scripts directly:"
	@echo "  scripts/build.sh {compile|release|clean}"
	@echo "  scripts/dev.sh {start|stop|restart|status|ping}"
	@echo "  scripts/deploy.sh {package}"

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
