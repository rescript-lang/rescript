# How to Create a Ralph Loop Setup

Ralph is an autonomous AI agent loop that runs repeatedly until all tasks are complete. Each iteration starts with fresh context, so progress must be tracked in files and git history.

## Core Concept

```bash
while true; do claude --dangerously-skip-permissions --print "$(cat PROMPT.md)"; done
```

> **Note:** `code` is an alias for `claude` with all permissions enabled.

The agent reads the same prompt each iteration, but sees different file state. Progress persists in:
1. **Files** - The TODO checklist, source code changes
2. **Git history** - Commits showing what's been done

## Files You Need

### 1. `RALPH_PROMPT.md` - Agent Instructions

This tells the agent what to do, how to do it, and when to stop.

```markdown
# Ralph Loop: [Task Name]

[Brief description of the goal]

## Core Principle

[Key constraints or approach requirements]

## Your Task

Read `RALPH_TODO.md` and complete the next unchecked task. After completing a task:
1. Mark it `[x]` complete in `RALPH_TODO.md`
2. Commit your changes with an atomic commit
3. If all tasks are complete, output "RALPH_COMPLETE"

## Critical Rules

1. [Rule 1 - e.g., always read reference code first]
2. [Rule 2 - e.g., run tests after each change]
3. [Rule 3 - e.g., commit after each fix]

## Key Files

| File | Purpose |
|------|---------|
| `path/to/main/file.rs` | Main implementation |
| `path/to/reference.ml` | Reference to study |

## Commands Reference

```bash
# Build
[build command]

# Test
[test command]

# Debug
[debug command]
```

## Exit Condition

When all tasks in `RALPH_TODO.md` are checked `[x]`, output:
```
RALPH_COMPLETE
```
```

### 2. `RALPH_TODO.md` - Progress Tracking

A checklist that the agent updates as it works. Organize by priority/phase.

```markdown
# [Task Name] TODO

**Last Updated:** [date]
**Status:** X/Y tasks complete

---

## Phase 1: [Category] (Highest Priority)

Explanation of why this phase is first.

- [ ] Task 1 description
- [ ] Task 2 description
- [ ] Task 3 description

---

## Phase 2: [Category]

- [ ] Task 4 description
- [ ] Task 5 description

---

## Commands Reference

```bash
[useful commands for the agent]
```

---

## Notes

[Any context that helps the agent understand the work]
```

## Best Practices

### 1. Organize by Root Cause, Not Symptoms

Bad:
```markdown
- [ ] Fix test file A
- [ ] Fix test file B
- [ ] Fix test file C
```

Good:
```markdown
## Phase 1: Fix Comment Handling (Root Cause)
- [ ] Study OCaml comment attachment algorithm
- [ ] Fix trailing comment preservation

## Phase 2: Test Files (Will Auto-Fix After Phase 1)
- [ ] test file A
- [ ] test file B
```

### 2. Include Reference Material

Always point the agent to authoritative sources:

```markdown
## Reference Implementation

Study these files before making changes:
- `path/to/reference/implementation.ml`
- `path/to/tests/expected/output.txt`
```

### 3. Make Progress Measurable

Include commands to measure progress:

```markdown
## Measuring Progress

```bash
# Run tests and count passing
./run_tests.sh 2>&1 | grep -E "passed|failed"

# Current baseline: 45/100 passing
# Target: 100/100 passing
```
```

### 4. Enforce Atomic Commits

The agent should commit after every improvement:

```markdown
## Commit Rules

- Commit after EVERY test that starts passing
- Use descriptive messages: "Fix: [specific thing fixed]"
- Never batch multiple unrelated fixes in one commit
```

### 5. Provide Debugging Commands

```markdown
## Debugging

```bash
# View diff for failing test
diff expected.txt actual.txt

# Test single file
./tool --test single_file.txt

# Compare with reference
./reference_tool file.txt > expected.txt
./our_tool file.txt > actual.txt
diff expected.txt actual.txt
```
```

### 6. Set Clear Exit Condition

```markdown
## Exit Condition

Output "RALPH_COMPLETE" when:
- All items in RALPH_TODO.md are checked [x]
- All tests pass (X/X)
- No regressions introduced
```

## Running the Loop

### Recommended: Bash Loop

Use the included `ralph.sh` script:

```bash
./ralph.sh                    # Uses RALPH_PROMPT.md by default
./ralph.sh MY_PROMPT.md       # Or specify a custom prompt file
```

The script:
- Tracks iteration count
- Logs timestamps for each iteration
- Detects `RALPH_COMPLETE` → exits successfully
- Detects `RALPH_BLOCKED` → exits with error for human intervention
- Fresh context each iteration (never fills up)

**Manual version** (if you want to customize):

```bash
while true; do
  cat RALPH_PROMPT.md | claude --dangerously-skip-permissions --print | tee /tmp/ralph_output.txt

  if grep -q "RALPH_COMPLETE" /tmp/ralph_output.txt; then
    echo "Ralph loop complete!"
    break
  fi
done
```

**Why bash loop over the plugin:**
- **Fresh context each iteration** - Escapes bad reasoning loops, never fills up
- **Unlimited iterations** - Can run for days on large task lists
- **Multiple exit conditions** - Can detect "COMPLETE" vs "BLOCKED" vs errors
- **Custom logic** - Add delays, notifications, logging between iterations
- **Progress in files** - All state is in RALPH_TODO.md + git, not in context

### Alternative: Claude Code Ralph Plugin

The plugin keeps context within a single session (doesn't clear between iterations):

```bash
/ralph-loop "$(cat RALPH_PROMPT.md)" --max-iterations 50 --completion-promise "RALPH_COMPLETE"
```

**When to use the plugin:**
- Short task lists (< 10 tasks) that fit in one context window
- Tasks where remembering previous attempts helps
- When you want the agent to build on its own reasoning

**Plugin limitations:**
- Context fills up on long tasks
- May get stuck in bad reasoning loops
- Only ONE completion string (exact match)

### Manual Iteration

For debugging or when you want full control:

```bash
# Run one iteration
cat RALPH_PROMPT.md | claude --dangerously-skip-permissions --print

# Check progress
cat RALPH_TODO.md | grep -E "^\- \[[ x]\]" | head -20

# Run another iteration
cat RALPH_PROMPT.md | claude --dangerously-skip-permissions --print
```

## Example: Setting Up a New Ralph Loop

### Step 1: Identify the Work

```bash
# Run tests to see what's failing
./run_tests.sh 2>&1 | tee test_output.txt

# Count failures by category
grep "FAIL" test_output.txt | cut -d: -f1 | sort | uniq -c
```

### Step 2: Create TODO with Phases

Group failures by root cause, not by file:

```bash
# Create RALPH_TODO.md
cat > RALPH_TODO.md << 'EOF'
# Migration TODO

**Status:** 0/50 complete

## Phase 1: Core Infrastructure (fixes 30% of failures)
- [ ] Implement missing utility function X
- [ ] Fix edge case in parser

## Phase 2: Individual Fixes
- [ ] Fix test A
- [ ] Fix test B
...
EOF
```

### Step 3: Create Prompt

```bash
cat > RALPH_PROMPT.md << 'EOF'
# Ralph Loop: [Your Task]

[Description and instructions...]

## Your Task
Read RALPH_TODO.md and complete the next unchecked task...

## Exit Condition
Output "RALPH_COMPLETE" when all tasks are done.
EOF
```

### Step 4: Run the Loop

```bash
while true; do cat RALPH_PROMPT.md | claude --dangerously-skip-permissions --print; done
```

## Tips

1. **Start small** - Test with 5-10 tasks before scaling up
2. **Monitor progress** - Check git log to see commits rolling in
3. **Intervene if stuck** - If the agent loops on the same task, add hints to the prompt
4. **Use phases** - Breaking work into phases helps the agent prioritize
5. **Trust git** - Progress is in commits, not in the agent's memory
6. **Fresh context is a feature** - Each iteration can approach problems fresh without being stuck in bad assumptions
