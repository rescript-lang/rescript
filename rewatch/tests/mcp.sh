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
