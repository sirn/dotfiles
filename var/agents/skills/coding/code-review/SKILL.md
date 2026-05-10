---
name: code-review
description: Review code for correctness, quality, conventions, simplicity, security, and overall health. Use when asked for a general or full code review.
---

Run a full code review or quality check.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.

2. Read relevant code and project guidance:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation and test files.
   - Prefer existing project patterns over external preferences.

3. Review the code:
   - Correctness: bugs, logic errors, edge cases, missing error handling, resource leaks, concurrency risks, and state-management issues.
   - Security: OWASP risks, injection flaws, auth/authz issues, sensitive data exposure, insecure dependencies, and unsafe defaults.
   - Conventions: naming, code organization, style consistency, documentation expectations, and project consistency.
   - Simplicity: over-engineering, unnecessary abstractions, dead code, clever logic, avoidable indirection, and no-code alternatives.
   - Best practices: verify framework/library/API usage with official documentation when uncertain.
   - Performance: call out obvious hot paths, N+1 queries, blocking I/O, avoidable allocations, or poor algorithmic complexity when relevant.

4. Run verification commands only when the user requested full checks:
   - Detect test commands from project instructions, task runners (`Makefile`, `justfile`, `Taskfile.yml`), wrapper scripts (`bin/`, `.my/bin/`), package manager scripts, then common defaults.
   - Detect lint/check/format commands from the same sources.
   - Run the most appropriate non-destructive commands with timeouts.
   - Report failures; only modify files if the user explicitly asked to fix issues.

5. Synthesize findings:
   - Prioritize real, evidenced issues over speculative concerns.
   - Include file paths and line references or quoted snippets.
   - Provide concrete fixes and verification steps.

## Output

1. **Executive Summary**
2. **Critical Issues** (must fix)
3. **Security Analysis**
4. **API / Documentation Verification**
5. **Quality & Logic**
6. **Simplicity & Convention**
7. **Performance & Best Practices**
8. **Test/Lint Results** (only when run)
9. **Quick Wins**
10. **Action Items** prioritized Critical > High > Medium > Low
