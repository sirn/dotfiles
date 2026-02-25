---
name: code-security-audit
description: Specific deep-dive security analysis. Use when asked to audit security or check for vulnerabilities.
---

Perform a focused security audit on specific components.

## Process
1. Identify context:
   - Target specific files, modules, or API endpoints mentioned by the user

2. Analyze for security vulnerabilities:
   - Check for OWASP Top 10 vulnerabilities
   - Check for injection flaws (SQLi, Command Injection, XSS)
   - Verify authentication and authorization logic
   - Identify sensitive data exposure risks
   - Check for insecure dependencies (use WebSearch/WebFetch for known CVEs)

3. Synthesize findings into a Security Report

## Output
1. **Executive Summary** (Risk Level)
2. **Vulnerability Analysis**
   - **Critical**: Exploitable flaws (Stop the line)
   - **High**: Likely exploitable or severe impact
   - **Medium**: Theoretical or difficult to exploit
   - **Low**: Best practice hardening
3. **Remediation Steps** (Code snippets or config changes)
4. **Verification** (How to test the fix)
