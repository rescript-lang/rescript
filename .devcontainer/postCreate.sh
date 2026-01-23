#!/bin/sh

# Install dev dependencies from OPAM
opam init -y --auto-setup --bare --disable-sandboxing
opam switch create 5.3.0
opam install . --with-test --with-dev-setup -y

# Add OPAM environment setup to shell startup script
echo 'eval $(opam env)' >> ~/.zshrc
echo 'eval $(opam env)' >> ~/.bashrc

nvm install

corepack enable
printf "\n" | yarn 
