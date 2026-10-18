#!/bin/sh

# Install dev dependencies from OPAM, with the overrides CI uses (Luv)
opam repository add rescript-overrides "$PWD/.github/opam-repository"
opam install . --with-test --with-dev-setup -y

corepack enable
printf "\n" | yarn 
