#!/bin/bash
# Deployment and packaging script

set -e

cd "$(dirname "$0")/.."

case "$1" in
    package)
        echo "Building release tarball for deployment..."
        rebar3 tar
        TARBALL=$(find _build/default/rel -name "*.tar.gz" | head -1)
        if [ -f "$TARBALL" ]; then
            echo ""
            echo "✓ Package created successfully:"
            echo "  $TARBALL"
            echo ""
            ls -lh "$TARBALL"
            echo ""
            echo "Deployment instructions:"
            echo "  1. Copy tarball to server:"
            echo "     scp $TARBALL user@server:/opt/"
            echo ""
            echo "  2. On server, extract and run:"
            echo "     tar -xzf hello_erlang-0.1.0.tar.gz"
            echo "     ./bin/hello_erlang daemon"
        else
            echo "Error: Tarball creation failed"
            exit 1
        fi
        ;;
    *)
        echo "Usage: $0 {package}"
        echo ""
        echo "Deployment commands:"
        echo "  package - Build tarball for deployment"
        echo ""
        echo "Future commands:"
        echo "  push-s3 - Upload to AWS S3"
        echo "  deploy  - Deploy to staging/production"
        exit 1
        ;;
esac
