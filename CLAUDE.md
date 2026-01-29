# Claude Code Instructions

Please read and follow the instructions in [AGENTS.md](./AGENTS.md).

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
