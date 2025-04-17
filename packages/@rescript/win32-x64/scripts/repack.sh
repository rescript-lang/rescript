#!/usr/bin/env bash

# Yarn doens't preserve executable permission bits of files as design decision.
#
# Workaround:

set -e

PACKAGE_TGZ="$1"

if [[ -z "$PACKAGE_TGZ" ]]; then
  echo "Usage: $0 path/to/package.tgz"
  exit 1
fi

TMP_DIR="$(mktemp -d)"

tar -xzf "$PACKAGE_TGZ" -C "$TMP_DIR"

chmod +x "$TMP_DIR/package/bin/"*

tar -czf "$PACKAGE_TGZ" -C "$TMP_DIR" package

echo "Repacked successfully"
