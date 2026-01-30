# Claude Code Instructions

Please read and follow the instructions in [AGENTS.md](./AGENTS.md).

## ⚠️ CRITICAL: COMMIT YOUR CHANGES

**You MUST commit after completing any meaningful work.** This is non-negotiable.

### When to commit:
- ✅ After fixing a bug or issue
- ✅ After implementing a feature or part of a feature
- ✅ After refactoring code
- ✅ After improving parity (even 1 more test passing)
- ✅ Before switching to a different task
- ✅ Before investigating a problem (commit WIP first)
- ✅ At natural stopping points

### How to commit:
```bash
# Stage specific files (preferred)
git add path/to/changed/files

# Commit with descriptive message
git commit -m "$(cat <<'EOF'
Short description of what changed

Co-Authored-By: Claude <noreply@anthropic.com>
EOF
)"
```

**DO NOT** leave work uncommitted. If you made changes, commit them.

## CRITICAL: Protect Uncommitted Work

**NEVER revert or discard uncommitted changes.** This has caused lost work multiple times.

When you have uncommitted changes and encounter issues:

1. **ALWAYS commit work-in-progress first** before any troubleshooting:
   ```bash
   git add -A && git commit -m "WIP: [description of current state]"
   ```

2. **NEVER run these commands when there are uncommitted changes:**
   - `git checkout -- <file>`
   - `git checkout HEAD -- <path>`
   - `git restore <file>`
   - `git reset --hard`
   - `git stash` (unless you immediately `git stash pop`)

3. **Make small, frequent commits** - every meaningful change should be committed, even if incomplete. Use "WIP:" prefix for work-in-progress commits.

4. **If tests fail or there are regressions**, commit the current state first, THEN investigate. You can always revert a commit, but you cannot recover uncommitted changes.
