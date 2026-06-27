---
name: code-fix-test
description: Diagnose and fix test, lint, or validation failures. Use only when explicitly asked to fix failing tests, lint, formatting, or check commands.
---

Diagnose and fix validation failures with minimal targeted changes.

## Process

### Step 1 - Identify Context

Read any provided failure output fully before making changes.
If code changes are present, run `jj diff -s` to view changed files, and use `jj diff -- path` to inspect specific files/directories.
Focus on user-specified files, commands, or failures.

### Step 2 - Reproduce Failure

Prefer the exact failing command from the user or project output.
If no command is provided, detect the relevant test/lint/check command from project instructions, task runners, package manager scripts, or common defaults.
Use appropriate timeouts and avoid watch modes.

### Step 3 - Research Root Cause

Spawn the `research` subagent:

```
Research the root cause of this failure:
{error output}

Identify whether this is:
- a product-code bug
- a test bug
- an environment issue
- a stale expectation

Find relevant docs or known issues.
```

### Step 4 - Apply Fix

Spawn the `worker` subagent:

```
Apply a minimal fix for the following in the specified file:
{root cause}
{file}

Do not weaken assertions, skip tests, or broaden ignores unless explicitly approved with sound rationale.
```

### Step 5 - Verify

Verify the fix using relevant commands.

### Step 6 - Stop Condition

- If a fix fails twice, stop, analyze the root cause, and ask for guidance.

### Step 6 - Report

Report the outcome to the user:

1. **Failing Command**
2. **Root Cause**
3. **Fix Applied**
4. **Verification Result**
5. **Remaining Issues** requiring manual action
