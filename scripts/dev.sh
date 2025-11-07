#!/bin/bash
# Local development server management

set -e

cd "$(dirname "$0")/.."

RELEASE_BIN="_build/default/rel/hello_erlang/bin/hello_erlang"
PORT=8080

check_release() {
    if [ ! -f "$RELEASE_BIN" ]; then
        echo "Error: Release not found. Please run 'scripts/build.sh release' first."
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
    start)
        check_release
        if is_running; then
            PID=$($RELEASE_BIN pid 2>/dev/null)
            echo "Error: hello_erlang is already running (PID: $PID)"
            echo "Use 'scripts/dev.sh restart' to restart the server"
            exit 1
        fi
        echo "Starting hello_erlang in daemon mode..."
        $RELEASE_BIN daemon
        ;;
    stop)
        check_release
        if ! is_running; then
            echo "hello_erlang is not running"
            exit 0
        fi
        echo "Stopping hello_erlang..."
        $RELEASE_BIN stop
        ;;
    restart)
        check_release
        echo "Restarting hello_erlang..."
        $RELEASE_BIN stop 2>/dev/null || true
        sleep 1
        $RELEASE_BIN daemon
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
            echo "hello_erlang is running (PID: $PID)"
        else
            echo "hello_erlang is not running"
        fi
        ;;
    ping)
        check_release
        if ! is_running; then
            echo "Error: hello_erlang is not running"
            echo "Start the server with 'scripts/dev.sh start' first"
            exit 1
        fi
        if [ -z "$2" ]; then
            echo "Usage: $0 ping <message>"
            echo "Example: $0 ping \"Hello World\""
            exit 1
        fi
        MESSAGE="$2"
        URL="http://localhost:${PORT}/echo?message=$(printf %s "$MESSAGE" | jq -sRr @uri)"
        echo "Pinging server with message: $MESSAGE"
        RESPONSE=$(curl -s "$URL")
        if [ $? -eq 0 ]; then
            echo "Response: $RESPONSE"
        else
            echo "Error: Could not connect to server at http://localhost:${PORT}"
            exit 1
        fi
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|foreground|console|status|ping <msg>}"
        echo ""
        echo "Local development server commands:"
        echo "  start      - Start server in daemon mode"
        echo "  stop       - Stop server"
        echo "  restart    - Restart server"
        echo "  foreground - Start in foreground mode"
        echo "  console    - Start with interactive console"
        echo "  status     - Check if server is running"
        echo "  ping <msg> - Test the echo endpoint"
        exit 1
        ;;
esac
