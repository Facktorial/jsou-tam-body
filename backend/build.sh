#!/bin/bash
set -e

if [ "$1" = "dev" ]; then
	echo "Building dev image..."
	docker build -t jsou-tam-body-api-dev -f Dockerfile.dev .
else
	echo "Building production image..."
	docker build -t jsou-tam-body-api -f Dockerfile .
fi
