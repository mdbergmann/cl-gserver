# syntax=docker/dockerfile:1
#
# Sento (cl-gserver) agent image for the AI development pipeline.
#
# Repo-specific image: the ai-pipeline repo stays language-agnostic, so the
# SBCL toolchain this project needs lives HERE. It is built FROM the pipeline's
# base agent image (ai-pipeline-agent:latest), so it keeps Claude Code, the
# entrypoint, git, jq, and the non-root uid-1000 user — and just adds SBCL plus
# ocicl (the OCI-based Common Lisp dependency manager).
#
# Build (the base image must exist first — `bin/build-images.sh` in ai-pipeline
# builds it). From this repo root:
#   .pipeline/build-image.sh
# or directly:
#   docker build -t cl-gserver-agent:latest -f .pipeline/Dockerfile .pipeline
#
# Then point .pipeline.yml at it:  image: cl-gserver-agent:latest
#
# Offline dependency resolution
# -----------------------------
# The pipeline's test stage runs with --network none, so dependencies must
# resolve with no network. This repo vendors every dependency under systems/
# (ocicl's local dist; present in the working tree the pipeline mounts at
# /workspace). CL_SOURCE_REGISTRY below makes ASDF search the repo root FIRST
# (so the working copy of sento wins over the older sento vendored at
# systems/cl-gserver-3.3.0/), then the whole systems/ tree for the deps. ASDF
# reads this env var with no init file, so every stage (review / fix / implement
# / test) resolves deps identically. ocicl is installed for the edit stages to
# ADD a dependency (`ocicl install <system>`), which needs egress to ghcr.io —
# see egress_allowlist in .pipeline.yml.
#
# Read-only root fs
# -----------------
# The container root fs is read-only; ASDF would otherwise write fasls under
# $HOME (a small tmpfs). ASDF_OUTPUT_TRANSLATIONS redirects all compiled output
# to /tmp/build, the exec-capable scratch tmpfs run-stage.sh provides (768m),
# keeping the read-only workspace read-only during the test stage.

FROM ai-pipeline-agent:latest

USER root

# --- SBCL + a C toolchain (for CFFI-grovelling libraries) ------------------
# Debian bookworm ships SBCL via apt (arch-correct on amd64 and arm64, where no
# official prebuilt binary exists). build-essential covers any dependency that
# grovels C at load time.
RUN apt-get update && apt-get install -y --no-install-recommends \
        sbcl \
        build-essential \
    && rm -rf /var/lib/apt/lists/*

# --- ocicl -----------------------------------------------------------------
# Built from source with the just-installed SBCL; ocicl vendors its own deps
# (ASDF, no Quicklisp), so the only network needed is the shallow git clone.
# OCICL_PREFIX puts the binary on the system PATH at /usr/local/bin/ocicl. We do
# NOT wire ocicl's runtime loader into a system init file on purpose: dependency
# resolution goes through CL_SOURCE_REGISTRY below, so even though this repo's
# systems.csv maps "sento" to the older systems/cl-gserver-3.3.0 copy, loading
# still picks up the working tree, not the stale copy.
RUN git clone --depth 1 https://github.com/ocicl/ocicl.git /tmp/ocicl \
    && cd /tmp/ocicl \
    && OCICL_PREFIX=/usr/local sbcl --non-interactive --disable-debugger --load setup.lisp \
    && command -v ocicl >/dev/null \
    && rm -rf /tmp/ocicl /home/agent/.cache /home/agent/.config /home/agent/.local

# --- ASDF / SBCL runtime config --------------------------------------------
# Resolve systems offline: repo root first (working copy wins), then the vendored
# systems/ tree. Redirect all fasl output to the exec-capable scratch tmpfs.
# (/workspace is the bind-mounted repo at run time; these are read lazily by ASDF.)
ENV CL_SOURCE_REGISTRY="/workspace/:/workspace/systems//" \
    ASDF_OUTPUT_TRANSLATIONS="/:/tmp/build/asdf-cache/"

USER agent
WORKDIR /workspace

ENTRYPOINT ["/opt/agent/entrypoint.sh"]
