---
name: code-review
description: Review code for correctness, quality, conventions, simplicity, security, and overall health. Use when asked for a general or full code review.
---

Run a full code review or quality check using specialized agents.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files or paths, focus on those.

### Step 2 - Spawn Review Agents

Spawn `reviewer` subagent:

```
Review {files} across all lenses:
- correctness/quality — bugs, logic errors, edge cases, error handling, resource leaks, concurrency, and performance traps
- security — OWASP risks, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults
- convention/simplicity — naming, organization, documentation, project consistency, over-engineering, unnecessary abstractions, dead code, and avoidable indirection
```

Spawn `researcher` subagent:

```
Verify relevant API/library usage in {files} against official documentation and research best practices.
```

### Step 3 - Audit

For production-bound changes, spawn `auditor` subagent:

```
Audit {files} for production readiness: correctness, security, data loss, migration hazards, and rollback safety.
```

### Step 4 - Synthesize Findings

Read relevant code yourself to validate and synthesize agent findings.

### Step 5 - Run Verification

Run verification commands only when the user requested full checks:

- Detect test commands from project instructions, task runners (`Makefile`, `justfile`, `Taskfile.yml`), wrapper scripts (`bin/`, `.my/bin/`), package manager scripts, then common defaults.
- Detect lint/check/format commands from the same sources.
- Run the most appropriate non-destructive commands with timeouts.
- Report failures; only modify files if the user explicitly asked to fix issues.

### Step 6 - Report

Produce the review report with the following structure:

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

Prioritize real, evidenced issues over speculative concerns.
Include file paths and line references or quoted snippets.
Provide concrete fixes and verification steps.
