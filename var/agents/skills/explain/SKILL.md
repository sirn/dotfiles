---
name: explain
description: Explain in simple terms. Use when user asks to explain something or wants a walkthrough.
---

Explain the code at $ARGUMENTS (or currently selected).

## Process
1. Read and analyze the code
2. Spawn both agents in parallel using the Task tool:
   - Use `doc-researcher` agent: Look up documentation for external libraries/frameworks
   - Use `code-architect` agent: Identify patterns used and how they fit the codebase
3. Synthesize findings into a clear explanation

## Output
1. **Purpose**: What this code does (1-2 sentences)
2. **How it works**: Key components and data flow
3. **Patterns**: Which patterns/architectural patterns are used
4. **Dependencies**: External libraries/APIs used (with doc links if researched)
5. **Gotchas**: Edge case, common pitfall, or non-explicit behavior

Make it understandable for someone unfamiliar with the codebase.
