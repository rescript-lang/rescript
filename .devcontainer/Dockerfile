FROM mcr.microsoft.com/devcontainers/rust:1-1-bookworm
LABEL org.opencontainers.image.authors="Christoph Knittel <ck@cca.io>"
LABEL org.opencontainers.image.description="Docker image for ReScript development."

RUN sudo apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    cmake \
    ninja-build \
    musl-tools \
    python-is-python3 \
    && rm -rf /var/lib/apt/lists/*

# Install OPAM
ARG OPAM_VERSION=2.4.1
RUN printf '\n\n' | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version $OPAM_VERSION"
