---
name: troubleshoot
description: Debug issues by researching errors and proposing minimal fixes. Use when user asks to troubleshoot or debug a failure.
---

Troubleshoot a problem by delegating research to the troubleshooter agent.

## Process
1. Identify the error message, log, or failure symptom from $ARGUMENTS or context
2. Spawn the `troubleshooter` agent using the Task tool
3. Synthesize findings into actionable steps

## Agent Invocation
- `troubleshooter`: "Investigate the error or failure in {context} and propose a minimal fix with sources"

## Output
1. **Likely cause**
2. **Evidence** (links or doc references)
3. **Fix**
4. **Verify**
