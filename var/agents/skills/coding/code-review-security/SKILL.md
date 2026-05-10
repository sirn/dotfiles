---
name: code-review-security
description: Perform focused security audits. Use when asked for security review, vulnerability review, threat analysis, or secure implementation checks.
---

Run a focused security review.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, paths, assets, trust boundaries, or threat models, focus on those.

2. Read relevant code and project guidance:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation, configuration, dependency, and test files.
   - Identify inputs, outputs, trust boundaries, permissions, secrets, and data sensitivity.

3. Review for security risks:
   - Check OWASP Top 10 risks, injection flaws, authentication and authorization, cryptography, sensitive data handling, dependency risks, and secure defaults.
   - Check unsafe deserialization, path traversal, SSRF, XSS/CSRF, command injection, SQL/NoSQL injection, sandbox escapes, and privilege escalation where relevant.
   - Distinguish exploitable issues from theoretical hardening suggestions.
   - Prefer minimal concrete remediations that fit project conventions.

4. Research when needed:
   - Verify framework-specific security guidance, advisories, and version-specific behavior with authoritative documentation.

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
