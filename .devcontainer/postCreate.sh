#!/bin/sh

# Install dev dependencies from OPAM
opam init -y --bare --disable-sandboxing
opam switch create 5.3.0 --packages ocaml-option-static
opam install . --deps-only -y

# For IDE support, install the OCaml language server
opam install ocaml-lsp-server -y

# Add OPAM environment setup to shell startup script
echo 'eval $(opam env)' >> ~/.zshrc
echo 'eval $(opam env)' >> ~/.bashrc

npm install --ignore-scripts
