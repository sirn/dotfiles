---
name: code-verify
description: Verify API usage against official documentation. Use when user asks to validate API correctness.
---

Audit API usage against authoritative documentation.

## Process
1. Identify the APIs, versions, or libraries in use from $ARGUMENTS or context
2. Spawn the `code-researcher` agent using the Task tool
3. Summarize mismatches, deprecated usage, or required configuration

## Agent Invocation
- `code-researcher`: "Verify API usage in {context} against official documentation and note discrepancies"

## Output
1. **Confirmed Correct Usage**
2. **Issues or Mismatches**
3. **Required Changes**
4. **Sources**
