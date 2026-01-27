---
name: update-daemon-docs
description: Reminder to update EXPERIMENT_DAEMON.md after making daemon-related changes. Use this skill proactively after modifying files in tests/daemon_tests/, rewatch/src/daemon/, rewatch/src/client/, or rewatch/proto/.
---

# Update Daemon Documentation

After making changes to daemon-related code or tests, you should update `rewatch/EXPERIMENT_DAEMON.md` to keep the documentation current.

## Prerequisites

First, check if `EXPERIMENT_DAEMON.md` exists - you might be on a different branch where it doesn't exist:

```bash
test -f rewatch/EXPERIMENT_DAEMON.md && echo "File exists" || echo "File does not exist"
```

If the file doesn't exist, skip this skill.

## What to Update

Based on your changes, update the relevant sections:

### If you added/modified daemon tests (`tests/daemon_tests/`):

1. **Test files table** (around line 565-590): Add or update the entry for your test file
2. **Future Test Scenarios** (around line 690): Move items from "Not yet covered" to "Covered scenarios" if applicable

### If you modified daemon code (`rewatch/src/daemon/`):

1. **How It Works** section: Update relevant subsections
2. **File Structure**: Update if you added new files
3. **DaemonEvent Variants**: Update if you added new events

### If you modified client code (`rewatch/src/client/`):

1. **Client Module** section: Update descriptions
2. **File Structure**: Update if you added new files

### If you modified the proto file (`rewatch/proto/rescript.proto`):

1. **DaemonEvent Variants**: Update the event list
2. **Event System** section: Update if the event architecture changed

## Guiding Principles

From the document itself:

> **Every code change to daemon/client architecture should have a matching update to this document.**

### Current State Only

The document reflects the **absolute latest state** - not history. When updating:

- **Do NOT** keep completed todo items or historical notes
- **Do NOT** add "done" markers or timestamps
- **Remove** items from "Not yet covered" when they become covered - don't leave them with a checkmark
- The document should read as if it was written fresh today describing the current system
- If a feature is implemented, it belongs in the "Covered scenarios" list, not crossed off in "Not yet covered"

Keep descriptions concise - bullet points over prose, no fluff.
