---
name: code-debug
description: Debug issues by researching errors and proposing minimal fixes. Use when user asks to troubleshoot or debug a failure.
---

Troubleshoot a problem by delegating focused research and risk review.

## Process

### Step 1 - Identify Error

Identify the error message, log, or failure symptom from the user's request or context.

### Step 2 - Research Error

Spawn `researcher` subagent:

```
Investigate the error or failure in {context}. Find likely root causes, official docs or known issues, minimal fixes, and verification steps with sources.
```

### Step 3 - Synthesize Findings

Synthesize findings into actionable steps.

### Step 4 - Apply Fix and Verify

If fixing is requested, make the smallest change that addresses the likely root cause and verify with the most specific command.

## Stop Condition

- If a proposed fix fails twice, stop, re-evaluate the diagnosis, and ask for guidance.

### Step 5 - Report

Report the following to the user:

1. **Likely cause**
2. **Evidence** (links or doc references)
3. **Fix**
4. **Verify**
