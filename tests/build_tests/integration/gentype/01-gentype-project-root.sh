#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
source "../utils.sh"

bold "Test: GenType resolves modules through a non-canonical project path"
node ./project-root.mjs
success "GenType project-root checks completed"
