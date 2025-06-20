FROM mcr.microsoft.com/devcontainers/rust:1-1-bookworm
LABEL org.opencontainers.image.authors="Christoph Knittel <ck@cca.io>"
LABEL org.opencontainers.image.description="Docker image for ReScript development."

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    yarn \
    opam \
    musl-tools \
    python-is-python3 \
    && rm -rf /var/lib/apt/lists/*
