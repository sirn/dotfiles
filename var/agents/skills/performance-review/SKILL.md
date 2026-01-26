---
name: performance-review
description: Review performance risks and quick wins. Use when user asks about performance.
---

Review for performance risks and suggest targeted improvements.

## Process
1. Identify performance-critical code paths
2. Spawn `best-practices-researcher` agent to research optimization techniques for the specific language/framework
3. Provide concrete optimization suggestions

## Focus Areas
- Algorithmic complexity
- Memory usage and allocations
- I/O operations and batching
- Network calls and connection pooling
- Caching opportunities

## Output
For each issue found:
1. **Problem**: What's slow and why
2. **Current code**: The problematic section
3. **Optimized solution**: The improved version
4. **Why it's better**: With benchmarks/docs if researched
