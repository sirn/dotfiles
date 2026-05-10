---
name: code-review-security
description: Perform focused security audits using specialized subagents when available. Use when asked for security review, vulnerability review, threat analysis, or secure implementation checks.
---

Run a focused security review using specialized agents.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, paths, assets, trust boundaries, or threat models, focus on those.

2. Spawn applicable agents in parallel:
   - `reviewer`: "Perform a focused security audit of {files}: OWASP Top 10, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults."
   - `researcher`: "Research official security guidance, advisories, and framework-specific secure implementation patterns relevant to {files}."

3. Read relevant code yourself to validate and synthesize agent findings:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation, configuration, dependency, and test files.
   - Identify inputs, outputs, trust boundaries, permissions, secrets, and data sensitivity.

4. Review for security risks:
   - Check unsafe deserialization, path traversal, SSRF, XSS/CSRF, command injection, SQL/NoSQL injection, sandbox escapes, and privilege escalation where relevant.
   - Distinguish exploitable issues from theoretical hardening suggestions.
   - Prefer minimal concrete remediations that fit project conventions.

5. Synthesize findings:
   - Prioritize real, evidenced issues over speculative concerns.
   - Include file paths and line references or quoted snippets.
   - Provide concrete fixes and verification steps.

## Output

1. **Executive Summary**
2. **Critical Vulnerabilities** (must fix)
3. **Security Findings** prioritized Critical > High > Medium > Low
4. **Evidence** with file paths and line references
5. **Recommended Remediation**
6. **Verification Steps**
7. **Hardening Ideas** (clearly separated from exploitable issues)
