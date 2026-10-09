#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: GenType resolves modules through a Windows short project path"
node ./project-root.mjs
success "GenType resolved the short project path"
