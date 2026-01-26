#!/bin/sh

# Add OPAM environment setup to shell startup script
echo 'eval $(opam env)' >> ~/.zshrc
echo 'eval $(opam env)' >> ~/.bashrc

eval $(opam env)

# Install dev dependencies from OPAM
opam install . --with-test --with-dev-setup -y

nvm install

corepack enable
printf "\n" | yarn 
