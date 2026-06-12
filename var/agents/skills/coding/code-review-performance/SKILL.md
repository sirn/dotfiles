---
name: code-review-performance
description: Review code for performance bottlenecks, algorithmic complexity, memory usage, and unnecessary work. Use when asked for performance review or optimization opportunities.
---

Run a focused performance review using specialized agents.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, paths, workloads, benchmarks, or symptoms, focus on those.
   - Identify suspected hot paths and expected scale.

2. Spawn applicable agents in parallel:
   - `reviewer`:
     ```text
     Review {files} with a performance lens: bottlenecks, algorithmic complexity, memory usage, blocking I/O, N+1 queries, unnecessary work, and concrete optimizations.
     ```
   - `researcher`:
     ```text
     Research framework/runtime/database-specific performance guidance relevant to {files}.
     ```

3. Read relevant code yourself to validate and synthesize agent findings:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation, query, allocation-heavy, concurrency, I/O, and test/benchmark files.
   - Prefer existing project patterns over speculative rewrites.

4. Review performance characteristics:
   - Distinguish measured or highly plausible bottlenecks from premature optimization.
   - Suggest concrete optimizations, measurement approaches, and benchmarks when useful.
   - Call out tradeoffs, readability costs, and operational risks.

5. Synthesize findings:
   - Prioritize issues by likely impact and confidence.
   - Include file paths and line references or quoted snippets.
   - Provide concrete fixes and verification steps.

## Output

1. **Executive Summary**
2. **Likely Hot Paths**
3. **Findings** prioritized Critical > High > Medium > Low
4. **Evidence and Confidence**
5. **Recommended Optimization**
6. **Measurement / Benchmark Plan**
7. **Tradeoffs and Risks**
