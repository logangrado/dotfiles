# Claude Hatchery Sandbox
# Minimal image for running Claude Code in an isolated environment.
# Uncomment the sections below to add tools your project needs.
FROM debian:trixie-slim

# ── System packages ───────────────────────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    curl \
    ca-certificates \
    jq \
    make \
    unzip \
    zip \
    && rm -rf /var/lib/apt/lists/*

# ── Non-root user (required for --dangerously-skip-permissions) ───────────────
RUN useradd -m -s /bin/bash claude
USER claude
ENV HOME=/home/claude
ENV PATH="/home/claude/.npm-global/bin:/home/claude/.local/bin:$PATH"

# ── Claude Code ───────────────────────────────────────────────────────────────
RUN curl -fsSL https://claude.ai/install.sh | bash

# =============================================================================
# Optional extras — uncomment what you need
# =============================================================================

# ── Python / uv ───────────────────────────────────────────────────────────────
# RUN curl -Ls https://astral.sh/uv/install.sh | sh

# ── Python / Miniconda ────────────────────────────────────────────────────────
# RUN curl -Ls https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
#     -o /tmp/miniconda.sh && \
#     bash /tmp/miniconda.sh -b -p /home/claude/miniconda && \
#     rm /tmp/miniconda.sh
# ENV PATH="/home/claude/miniconda/bin:$PATH"

# ── Rust / cargo ──────────────────────────────────────────────────────────────
# RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
# ENV PATH="/home/claude/.cargo/bin:$PATH"

# ── Go ────────────────────────────────────────────────────────────────────────
# USER root
# RUN curl -Ls https://go.dev/dl/go1.23.0.linux-amd64.tar.gz | tar -C /usr/local -xz
# USER claude
# ENV PATH="/usr/local/go/bin:$PATH"

# ── Extra apt packages ────────────────────────────────────────────────────────
# USER root
# RUN apt-get update && apt-get install -y --no-install-recommends \
#     build-essential \
#     jq \
#     postgresql-client \
#     && rm -rf /var/lib/apt/lists/*
# USER claude

# =============================================================================

WORKDIR /repo
