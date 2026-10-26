#!/bin/sh

# Install dev dependencies from OPAM
opam install . --with-test --with-dev-setup -y

# The Node feature provides yarn: it enables corepack where Node still bundles
# it (Node 24 and older) and installs yarn with npm otherwise.
printf "\n" | yarn 
