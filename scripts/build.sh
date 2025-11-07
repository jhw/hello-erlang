#!/bin/bash
# Build management script

set -e

cd "$(dirname "$0")/.."

case "$1" in
    compile)
        echo "Compiling Erlang code..."
        rebar3 compile
        ;;
    release)
        echo "Building release..."
        rebar3 release
        echo "Release built: _build/default/rel/hello_erlang"
        ;;
    clean)
        echo "Cleaning build artifacts..."
        rebar3 clean
        rm -rf _build
        echo "Clean complete."
        ;;
    *)
        echo "Usage: $0 {compile|release|clean}"
        echo ""
        echo "Commands:"
        echo "  compile - Compile Erlang code"
        echo "  release - Build release"
        echo "  clean   - Remove all build artifacts"
        exit 1
        ;;
esac
