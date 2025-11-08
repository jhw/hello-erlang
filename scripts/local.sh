#!/bin/bash
# Local development script - build and run locally

set -e

cd "$(dirname "$0")/.."

RELEASE_BIN="_build/default/rel/hello_erlang/bin/hello_erlang"
PORT=8080

usage() {
    echo "Usage: $0 {compile|release|clean|start|stop|restart|console|status|ping} [options]"
    echo ""
    echo "Build commands:"
    echo "  compile    - Compile Erlang code"
    echo "  release    - Build release"
    echo "  clean      - Remove all build artifacts"
    echo ""
    echo "Server commands:"
    echo "  start      - Start server in daemon mode"
    echo "  stop       - Stop server"
    echo "  restart    - Restart server"
    echo "  foreground - Start in foreground mode"
    echo "  console    - Start with interactive console"
    echo "  status     - Check if server is running"
    echo "  ping <msg> - Test the echo endpoint"
    echo ""
    echo "Examples:"
    echo "  $0 release        # Build release"
    echo "  $0 console        # Start with console"
    echo "  $0 ping Hello     # Test endpoint"
    exit 1
}

check_release() {
    if [ ! -f "$RELEASE_BIN" ]; then
        echo "Error: Release not found. Please run '$0 release' first."
        exit 1
    fi
}

is_running() {
    set +e
    PID=$($RELEASE_BIN pid 2>/dev/null)
    EXIT_CODE=$?
    set -e
    if [ $EXIT_CODE -eq 0 ] && [ ! -z "$PID" ]; then
        return 0  # Running
    else
        return 1  # Not running
    fi
}

case "$1" in
    # Build commands
    compile)
        echo "Compiling Erlang code..."
        rebar3 compile
        ;;
    release)
        echo "Building release..."
        rebar3 release
        echo "✓ Release built: _build/default/rel/hello_erlang"
        ;;
    clean)
        echo "Cleaning build artifacts..."
        rebar3 clean
        rm -rf _build
        echo "✓ Clean complete"
        ;;

    # Server commands
    start)
        check_release
        if is_running; then
            PID=$($RELEASE_BIN pid 2>/dev/null)
            echo "Error: hello_erlang is already running (PID: $PID)"
            echo "Use '$0 restart' to restart the server"
            exit 1
        fi
        echo "Starting hello_erlang in daemon mode..."
        $RELEASE_BIN daemon
        sleep 1
        if is_running; then
            PID=$($RELEASE_BIN pid 2>/dev/null)
            echo "✓ Server started (PID: $PID)"
            echo "  URL: http://localhost:${PORT}/echo?message=Hello"
        else
            echo "✗ Failed to start server"
            exit 1
        fi
        ;;
    stop)
        check_release
        if ! is_running; then
            echo "hello_erlang is not running"
            exit 0
        fi
        echo "Stopping hello_erlang..."
        $RELEASE_BIN stop
        echo "✓ Server stopped"
        ;;
    restart)
        check_release
        echo "Restarting hello_erlang..."
        $RELEASE_BIN stop 2>/dev/null || true
        sleep 1
        $RELEASE_BIN daemon
        sleep 1
        if is_running; then
            PID=$($RELEASE_BIN pid 2>/dev/null)
            echo "✓ Server restarted (PID: $PID)"
        else
            echo "✗ Failed to restart server"
            exit 1
        fi
        ;;
    foreground)
        check_release
        echo "Starting hello_erlang in foreground mode..."
        $RELEASE_BIN foreground
        ;;
    console)
        check_release
        echo "Starting hello_erlang with interactive console..."
        $RELEASE_BIN console
        ;;
    status)
        check_release
        if is_running; then
            PID=$($RELEASE_BIN pid 2>/dev/null)
            echo "✓ hello_erlang is running (PID: $PID)"
            echo "  URL: http://localhost:${PORT}/echo?message=Hello"
        else
            echo "✗ hello_erlang is not running"
        fi
        ;;
    ping)
        check_release
        if ! is_running; then
            echo "Error: hello_erlang is not running"
            echo "Start the server with '$0 start' first"
            exit 1
        fi
        if [ -z "$2" ]; then
            echo "Usage: $0 ping <message>"
            echo "Example: $0 ping \"Hello World\""
            exit 1
        fi
        MESSAGE="$2"
        URL="http://localhost:${PORT}/echo?message=$(printf %s "$MESSAGE" | jq -sRr @uri)"
        echo "Testing endpoint with message: $MESSAGE"
        RESPONSE=$(curl -s "$URL")
        if [ $? -eq 0 ]; then
            echo "✓ Response: $RESPONSE"
        else
            echo "✗ Error: Could not connect to server at http://localhost:${PORT}"
            exit 1
        fi
        ;;
    *)
        usage
        ;;
esac
