#!/usr/bin/env bash
#
# build-image.sh — build this repo's SBCL agent image for the AI pipeline.
#
# The image is built FROM the pipeline's base image (ai-pipeline-agent:latest),
# which the ai-pipeline repo produces via `bin/build-images.sh`. Run that once
# per machine first; rerun this whenever .pipeline/Dockerfile changes.
#
# Usage:
#   .pipeline/build-image.sh            # build cl-gserver-agent:latest
#   IMAGE=my:tag .pipeline/build-image.sh
#
# Keep the tag in sync with `image:` in .pipeline.yml.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE="${IMAGE:-cl-gserver-agent:latest}"
BASE_IMAGE="ai-pipeline-agent:latest"

command -v docker >/dev/null || { echo "ERROR: docker not found on PATH" >&2; exit 1; }
docker info >/dev/null 2>&1 || { echo "ERROR: docker daemon not reachable — is OrbStack/Docker running?" >&2; exit 1; }

docker image inspect "$BASE_IMAGE" >/dev/null 2>&1 || {
  echo "ERROR: base image '$BASE_IMAGE' not found." >&2
  echo "       Build it first in the ai-pipeline repo: bin/build-images.sh" >&2
  exit 1
}

echo ">>> building $IMAGE (FROM $BASE_IMAGE)" >&2
docker build -t "$IMAGE" -f "$HERE/Dockerfile" "$HERE"
echo ">>> done. Set 'image: $IMAGE' in .pipeline.yml" >&2
