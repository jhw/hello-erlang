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
    tar)
        echo "Building release tarball with ERTS (prod mode)..."
        rebar3 as prod tar
        TARBALL=$(find _build/prod/rel -name "*.tar.gz" | head -1)
        if [ -z "$TARBALL" ]; then
            echo "Error: Tarball not found"
            exit 1
        fi
        echo "Tarball built: $TARBALL"
        ;;
    clean)
        echo "Cleaning build artifacts..."
        rebar3 clean
        rm -rf _build
        echo "Clean complete."
        ;;
    *)
        echo "Usage: $0 {compile|release|tar|clean}"
        echo ""
        echo "Commands:"
        echo "  compile - Compile Erlang code"
        echo "  release - Build release"
        echo "  tar     - Build release tarball for deployment"
        echo "  clean   - Remove all build artifacts"
        exit 1
        ;;
esac
