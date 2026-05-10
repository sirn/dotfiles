---
name: code-review
description: Review code for quality, security, API correctness, and overall health using specialized subagents when available. Use when asked for code review, security audit, API verification, full quality checks, or to check code health.
---

Run a code review or quality check using specialized agents.

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

2. Spawn applicable agents in parallel:

   **Full / Quality**:
   - `reviewer`: "Review {files} with a correctness/quality lens: bugs, logic errors, edge cases, error handling, resource leaks, concurrency, and performance traps."
   - `reviewer`: "Review {files} with a security lens: OWASP risks, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults."
   - `reviewer`: "Review {files} with a convention/simplicity lens: naming, organization, documentation, project consistency, over-engineering, unnecessary abstractions, dead code, and avoidable indirection."
   - `researcher`: "Verify relevant API/library usage in {files} against official documentation and research best practices."

   **Fast**:
   - `reviewer`: "Review {files} with a correctness and simplicity lens: bugs, logic errors, edge cases, unnecessary complexity, and avoidable indirection."

   **Security**:
   - `reviewer`: "Perform a focused security audit of {files}: OWASP Top 10, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults."
   - `researcher`: "Research official security guidance, advisories, and framework-specific secure implementation patterns relevant to {files}."

   **API Verify**:
   - `researcher`: "Verify API/library usage in {files or context} against official documentation, including version-specific parameters, behavior, configuration, and deprecations."
   - `reviewer`: "Review the API/library usage findings for correctness risk and minimal remediation."

   **Performance**:
   - `reviewer`: "Review {files} with a performance lens: bottlenecks, algorithmic complexity, memory usage, blocking I/O, N+1 queries, and concrete optimizations."
   - `researcher`: "Research framework/runtime-specific performance guidance relevant to {files}."

3. Read relevant code yourself to validate and synthesize agent findings.

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
