---
name: code-debug
description: Debug issues by researching errors and proposing minimal fixes. Use when user asks to troubleshoot or debug a failure.
---

Troubleshoot a problem by delegating research to the code-debug-researcher agent.

## Process
1. Identify the error message, log, or failure symptom from the user's request or context
2. Spawn the `code-debug-researcher` agent using the Task tool
3. Synthesize findings into actionable steps

## Agent Invocation
- `code-debug-researcher`: "Investigate the error or failure in {context} and propose a minimal fix with sources"

## Output
1. **Likely cause**
2. **Evidence** (links or doc references)
3. **Fix**
4. **Verify**
