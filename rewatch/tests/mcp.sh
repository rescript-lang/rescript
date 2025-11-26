#!/bin/bash
set -e
cd $(dirname $0)
source ./utils.sh

bold "Test: MCP tools and diagnose"

# Ensure env like suite.sh (RESCRIPT_BSC_EXE/RESCRIPT_RUNTIME)
if [[ -z "$RESCRIPT_BSC_EXE" || -z "$RESCRIPT_RUNTIME" ]]; then
  eval $(node ./get_bin_paths.js)
  export RESCRIPT_BSC_EXE
  export RESCRIPT_RUNTIME
fi

# 1) tools/list contains diagnose
resp=$(printf '{"jsonrpc":"2.0","id":1,"method":"tools/list"}\n' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null)
echo "$resp" | grep -q '"name":"diagnose"' || { error "tools/list missing diagnose"; echo "$resp"; exit 1; }

success "tools/list contains expected tools"

# 2) diagnose valid file (should not be error)
resp=$(printf '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"diagnose","arguments":{"path":"../testrepo/src/Test.res"}}}\n' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null)
echo "$resp" | grep -q '"isError":true' && { error "diagnose on valid file returned error"; echo "$resp"; exit 1; }
echo "$resp" | grep -q '"text":"OK"' || { error "diagnose on valid file did not return OK"; echo "$resp"; exit 1; }

success "diagnose on valid file OK"

# 3) diagnose invalid file (should be error)
resp=$(printf '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"diagnose","arguments":{"path":"../testrepo/src/DoesNotExist.res"}}}\n' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null)
echo "$resp" | grep -q '"isError":true' || { error "diagnose on invalid file did not error"; echo "$resp"; exit 1; }

success "diagnose on invalid file errors as expected"

# 4) diagnose syntax error (should include diagnostic content)
tmpfile="../testrepo/src/Bad_mcp.res"
echo "let x = " > "$tmpfile"
resp=$(printf '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"diagnose","arguments":{"path":"testrepo/src/Bad_mcp.res"}}}\n' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null)
rm -f "$tmpfile"

echo "$resp" | grep -q '"content":\[' || { error "diagnose did not return content for syntax error"; echo "$resp"; exit 1; }

success "diagnose reports syntax errors with content"

bold "Test: MCP perform-action (single file)"

# ApplyAutomaticMigrationsForCurrentFile on a simple file (should no-op and return OK)
resp=$(cat <<'JSON' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"perform-action","arguments":{"path":"../testrepo/src/Test.res","actionId":"\"ApplyAutomaticMigrationsForCurrentFile\""}}}
JSON
)
echo "$resp" | grep -q '"isError":true' && { error "perform-action (single file) returned error"; echo "$resp"; exit 1; }
echo "$resp" | grep -q '"text":"OK"' || { error "perform-action (single file) did not return OK"; echo "$resp"; exit 1; }

success "perform-action (single file) OK"

bold "Test: MCP perform-action (full project)"

# ApplyAutomaticMigrationsForFullProject from a file path (server resolves project root); expect OK
resp=$(cat <<'JSON' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null
{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"perform-action","arguments":{"path":"../testrepo/src/Test.res","actionId":"\"ApplyAutomaticMigrationsForFullProject\""}}}
JSON
)
echo "$resp" | grep -q '"isError":true' && { error "perform-action (full project) returned error"; echo "$resp"; exit 1; }
echo "$resp" | grep -q '"text":"OK"' || { error "perform-action (full project) did not return OK"; echo "$resp"; exit 1; }

success "perform-action (full project) OK"

bold "Test: MCP perform-action accepts plain string actionId"

# Single file action with plain string (not stringified JSON)
resp=$(cat <<'JSON' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null
{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"perform-action","arguments":{"path":"../testrepo/src/Test.res","actionId":"ApplyAutomaticMigrationsForCurrentFile"}}}
JSON
)
echo "$resp" | grep -q '"isError":true' && { error "perform-action (plain string, single file) returned error"; echo "$resp"; exit 1; }
echo "$resp" | grep -q '"text":"OK"' || { error "perform-action (plain string, single file) did not return OK"; echo "$resp"; exit 1; }

# Project-wide action with plain string
resp=$(cat <<'JSON' | "$REWATCH_EXECUTABLE" mcp 2>/dev/null
{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"perform-action","arguments":{"path":"../testrepo/src/Test.res","actionId":"ApplyAutomaticMigrationsForFullProject"}}}
JSON
)
echo "$resp" | grep -q '"isError":true' && { error "perform-action (plain string, full project) returned error"; echo "$resp"; exit 1; }
echo "$resp" | grep -q '"text":"OK"' || { error "perform-action (plain string, full project) did not return OK"; echo "$resp"; exit 1; }

success "perform-action accepts plain string actionId"
