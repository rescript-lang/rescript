#!/bin/bash

# commit-msg hook: the commit message is a commit record
# (.claude/skills/records/SKILL.md). Every commit carries the required
# trailers Area, Output, Tests and Signed-off-by, with Signed-off-by last; a
# commit that changes the compiler, the runtime, the build system or the
# editor tooling also has a Why: section and at least one change block naming
# the object it changes. Enable it in a checkout with
#   ln -sf "$PWD/scripts/commitRecordHook.sh" "$(git rev-parse --git-path hooks)/commit-msg"

exec 1>&2

message_file=$1
git_dir=$(git rev-parse --git-dir)

# a merge commit has no record of its own
if [[ -f "$git_dir/MERGE_HEAD" ]]; then
    exit 0
fi

body=$(grep -v '^#' "$message_file")
# fixup! and squash! commits are folded into a recorded commit
case "$body" in
    fixup!*|squash!*|amend!*) exit 0 ;;
esac

trailers=$(printf '%s\n' "$body" | git interpret-trailers --parse)
value() {
    printf '%s\n' "$trailers" | sed -n "s/^$1: //p" | head -n 1
}

problems=()
for key in Area Output Tests Signed-off-by; do
    if ! printf '%s\n' "$trailers" | grep -q "^$key: "; then
        problems+=("no '$key:' trailer")
    fi
done
last=$(printf '%s\n' "$trailers" | tail -n 1)
if [[ -n "$last" && "$last" != Signed-off-by:* ]]; then
    problems+=("the last trailer is '${last%%:*}'; Signed-off-by is the last trailer")
fi

output=$(value Output)
case "$output" in
    unchanged*|changed*|"") ;;
    *) problems+=("Output '$output' starts with neither 'unchanged' nor 'changed'") ;;
esac
tests=$(value Tests)
case "$tests" in
    "none, because "?*|[+~-]?*|"") ;;
    none*) problems+=("Tests: none gives its reason as 'none, because <reason>'") ;;
    *) problems+=("Tests lists tests as +added ~changed -removed paths, or is 'none, because <reason>'") ;;
esac

if git diff --cached --name-only |
    grep -Eq '^(compiler|rewatch/src|analysis|tools/src|packages/@rescript/(runtime|belt))/'; then
    kinds='Syntax|Format|AST|Transform|Typing|Typedtree|Diagnostic|Matching|Lambda|IR|Representation|JS|API|TypeScript|Editor|Build|Semantics|Code'
    if ! printf '%s\n' "$body" | grep -q '^Why:'; then
        problems+=("no 'Why:' section (the problem this commit addresses)")
    fi
    if ! printf '%s\n' "$body" | grep -Eq "^($kinds) (added|modified|removed)( \(.*\))?:"; then
        problems+=("no change block such as 'Typing modified (rule name):' or 'Code added (module):'")
    fi
fi

if (( ${#problems[@]} )); then
    echo "The commit message is not a commit record:"
    for p in "${problems[@]}"; do
        echo "  - $p"
    done
    echo "Format: .claude/skills/records/SKILL.md. The trailers are the last block of"
    echo "the message, one 'Key: value' per line; git interpret-trailers --parse reads them."
    exit 1
fi
