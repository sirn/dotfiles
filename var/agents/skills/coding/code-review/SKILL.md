---
name: code-review
description: Review code for quality, security, API correctness, and overall health. Use when asked for code review, security audit, API verification, full quality checks, or to check code health.
---

Run a code review or quality check.

## Modes

- **Full** (default): Review correctness, security, conventions, simplicity, and best practices.
- **Fast**: Review correctness and simplicity only.
- **Security**: Focus on security vulnerabilities and remediation.
- **API Verify**: Verify API/library usage against authoritative documentation.
- **Quality**: Full review plus direct test/lint/check command execution when requested.
- **Performance**: Focus on performance bottlenecks, algorithmic complexity, and memory usage.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.
   - Determine mode from the request; default to **Full**.

2. Read relevant code and project guidance:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation and test files.
   - Prefer existing project patterns over external preferences.

3. Review by mode:

   **Full / Quality**:
   - Correctness: bugs, logic errors, edge cases, missing error handling, resource leaks.
   - Security: OWASP risks, injection flaws, auth/authz issues, sensitive data exposure, insecure dependencies.
   - Conventions: naming, code organization, style consistency, documentation expectations.
   - Simplicity: over-engineering, unnecessary abstractions, dead code, clever logic, avoidable indirection.
   - Best practices: verify framework/library/API usage with official documentation when uncertain.

   **Fast**:
   - Correctness and simplicity only.

   **Security**:
   - Check OWASP Top 10 risks, injection flaws, authentication and authorization, cryptography, sensitive data handling, dependency risks, and secure defaults.
   - Distinguish exploitable issues from theoretical hardening suggestions.

   **API Verify**:
   - Identify APIs, versions, or libraries in use.
   - Research official documentation with WebSearch/WebFetch.
   - Check version-specific changes, parameter names, types, expected behavior, and deprecations.
   - Compare usage against documentation.

   **Performance**:
   - Look for hot paths, unnecessary work, blocking I/O, N+1 queries, avoidable allocations, poor algorithmic complexity, and memory pressure.
   - Suggest concrete optimizations and benchmarks when useful.

4. For **Quality** mode only, run verification commands directly when the user requested full checks:
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
3. **Security Analysis** (Full, Security, or Quality mode)
4. **API / Documentation Verification** (API Verify, Full when relevant, or Quality mode)
5. **Quality & Logic**
6. **Simplicity & Convention**
7. **Performance & Best Practices**
8. **Test/Lint Results** (Quality mode only)
9. **Quick Wins**
10. **Action Items** prioritized Critical > High > Medium > Low
