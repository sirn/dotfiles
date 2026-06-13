---
name: code-fix-test
description: Diagnose and fix test, lint, or validation failures. Use only when explicitly asked to fix failing tests, lint, formatting, or check commands.
---

Diagnose and fix validation failures with minimal targeted changes.

## Process

### Step 1 - Identify Context

- If failure output is provided, read it fully before changing anything.
- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files, commands, or failures, focus on those.

### Step 2 - Reproduce Failure

- Prefer the exact failing command from the user or project output.
- If no command is provided, detect the relevant test/lint/check command from instructions, task runners, wrappers, package manager scripts, then common defaults.
- Use proper timeouts and avoid watch modes.

### Step 3 - Research Root Cause

Spawn `research` subagent:

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

Spawn `worker` subagent:

```
Apply a minimal fix for the following in the specified file:
{root cause}
{file}

Do not weaken assertions, skip tests, or broaden ignores unless explicitly approved with sound rationale.
```

### Step 5 - Verify

Verify using relevant methods.

### Step 6 - Stop Condition

- If a fix fails twice, stop, provide root-cause analysis, and ask for guidance.

### Step 6 - Report

Report the following to the user:

1. **Failing Command**
2. **Root Cause**
3. **Fix Applied**
4. **Verification Result**
5. **Remaining Issues** requiring manual action
