FROM mcr.microsoft.com/devcontainers/rust:2-1-trixie
LABEL org.opencontainers.image.authors="Christoph Knittel <ck@cca.io>"
LABEL org.opencontainers.image.description="Docker image for ReScript development."

RUN sudo apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    cmake \
    musl-tools \
    ninja-build \
    && rm -rf /var/lib/apt/lists/*

# Install OPAM
ARG OPAM_VERSION=2.5.0
RUN printf '\n\n' | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version $OPAM_VERSION"

USER vscode
ENV HOME=/home/vscode

RUN opam init -y --shell-setup --enable-shell-hook --compiler=5.3.0 --disable-sandboxing && echo 'eval $(opam env)' >> ~/.zshrc && echo 'eval $(opam env)' >> ~/.bashrc