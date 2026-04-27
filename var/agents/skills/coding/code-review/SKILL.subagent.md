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
   - `quality-reviewer`: "Review {files} for bugs, logic errors, edge cases, and error handling issues."
   - `security-researcher`: "Review {files} for security vulnerabilities, OWASP risks, auth/authz issues, and sensitive data exposure."
   - `convention-reviewer`: "Review {files} for naming, organization, documentation, and project consistency."
   - `simplicity-reviewer`: "Review {files} for over-engineering, unnecessary abstractions, dead code, clever logic, and avoidable indirection."
   - `code-researcher`: "Verify relevant API/library usage in {files} against official documentation and research best practices."

   **Fast**:
   - `quality-reviewer`: "Review {files} for bugs, logic errors, edge cases, and error handling issues."
   - `simplicity-reviewer`: "Review {files} for over-engineering, unnecessary complexity, and avoidable indirection."

   **Security**:
   - `security-researcher`: "Perform a focused security audit of {files}: OWASP Top 10, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults."

   **API Verify**:
   - `code-researcher`: "Verify API/library usage in {files or context} against official documentation, including version-specific parameters, behavior, configuration, and deprecations."

   **Performance**:
   - `code-researcher`: "Analyze {files} for performance bottlenecks, algorithmic complexity, memory usage, blocking I/O, N+1 queries, and concrete optimizations."

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
