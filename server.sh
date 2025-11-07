#!/bin/bash
# Convenience script for managing the hello_erlang release

RELEASE_BIN="_build/default/rel/hello_erlang/bin/hello_erlang"
PORT=8080

case "$1" in
    build)
        echo "Building release..."
        rebar3 release
        ;;
    clean)
        echo "Cleaning build artifacts..."
        rebar3 clean
        rm -rf _build
        echo "Clean complete."
        ;;
    start)
        if [ ! -f "$RELEASE_BIN" ]; then
            echo "Error: Release not found. Please run './server.sh build' first."
            exit 1
        fi
        echo "Starting hello_erlang in daemon mode..."
        $RELEASE_BIN daemon
        ;;
    stop)
        echo "Stopping hello_erlang..."
        $RELEASE_BIN stop
        ;;
    restart)
        if [ ! -f "$RELEASE_BIN" ]; then
            echo "Error: Release not found. Please run './server.sh build' first."
            exit 1
        fi
        echo "Restarting hello_erlang..."
        $RELEASE_BIN daemon_attach
        $RELEASE_BIN stop
        $RELEASE_BIN daemon
        ;;
    foreground|fg)
        if [ ! -f "$RELEASE_BIN" ]; then
            echo "Error: Release not found. Please run './server.sh build' first."
            exit 1
        fi
        echo "Starting hello_erlang in foreground mode..."
        $RELEASE_BIN foreground
        ;;
    console)
        if [ ! -f "$RELEASE_BIN" ]; then
            echo "Error: Release not found. Please run './server.sh build' first."
            exit 1
        fi
        echo "Starting hello_erlang with interactive console..."
        $RELEASE_BIN console
        ;;
    status)
        if [ ! -f "$RELEASE_BIN" ]; then
            echo "Error: Release not found. Please run './server.sh build' first."
            exit 1
        fi
        echo "Checking hello_erlang status..."
        $RELEASE_BIN pid 2>/dev/null
        if [ $? -eq 0 ]; then
            echo "hello_erlang is running (PID: $($RELEASE_BIN pid))"
        else
            echo "hello_erlang is not running"
        fi
        ;;
    ping)
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
        echo "Usage: $0 {build|clean|start|stop|restart|foreground|console|status|ping}"
        echo ""
        echo "Commands:"
        echo "  build      - Build the release using rebar3"
        echo "  clean      - Clean all build artifacts"
        echo "  start      - Start the server in daemon mode"
        echo "  stop       - Stop the server"
        echo "  restart    - Restart the server"
        echo "  foreground - Start in foreground mode (fg also works)"
        echo "  console    - Start with an interactive Erlang console"
        echo "  status     - Check if the server is running"
        echo "  ping <msg> - Send a message to the echo endpoint"
        exit 1
        ;;
esac
