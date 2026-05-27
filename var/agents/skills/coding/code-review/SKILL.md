---
name: code-review
description: Review code for correctness, quality, conventions, simplicity, security, and overall health. Use when asked for a general or full code review.
---

Run a full code review or quality check using specialized agents.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.

2. Spawn applicable agents in parallel:
   - `reviewer`: "Review {files} with a correctness/quality lens: bugs, logic errors, edge cases, error handling, resource leaks, concurrency, and performance traps."
   - `reviewer`: "Review {files} with a security lens: OWASP risks, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults."
   - `reviewer`: "Review {files} with a convention/simplicity lens: naming, organization, documentation, project consistency, over-engineering, unnecessary abstractions, dead code, and avoidable indirection."
   - `researcher`: "Verify relevant API/library usage in {files} against official documentation and research best practices."

3. For production-bound changes, spawn `auditor`:
   - `auditor`: "Audit {files} for production readiness: correctness, security, data loss, migration hazards, and rollback safety."

4. Read relevant code yourself to validate and synthesize agent findings.

5. Run verification commands only when the user requested full checks:
   - Detect test commands from project instructions, task runners (`Makefile`, `justfile`, `Taskfile.yml`), wrapper scripts (`bin/`, `.my/bin/`), package manager scripts, then common defaults.
   - Detect lint/check/format commands from the same sources.
   - Run the most appropriate non-destructive commands with timeouts.
   - Report failures; only modify files if the user explicitly asked to fix issues.

6. Synthesize findings:
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
