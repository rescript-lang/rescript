#!/bin/sh

# Install dev dependencies from OPAM
opam install . --with-test --with-dev-setup -y

nvm install

corepack enable
printf "\n" | yarn 
