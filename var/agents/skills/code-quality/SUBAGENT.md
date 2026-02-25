---
name: code-quality
description: Run comprehensive quality checks by orchestrating review, verification, testing, and linting.
---

Run comprehensive quality checks by orchestrating specialized agents in parallel.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those

2. Spawn all five reviewer agents in parallel using the Task tool:
   - `quality-reviewer`: "Review {files} for bugs, logic errors, and edge cases"
   - `security-researcher`: "Review {files} for security vulnerabilities and OWASP risks"
   - `convention-reviewer`: "Review {files} for naming and code organization consistency"
   - `simplicity-reviewer`: "Review {files} for over-engineering and unnecessary complexity"
   - `code-researcher`: "Verify API usage in {files} against official documentation and research best practices"

3. Wait for all agents to complete and collect their findings
4. Also invoke `code-test` skill and `code-lint` skill to get test and lint results
5. Consolidate all findings into a single report

## Output
1. **Summary** - Overall code health assessment (including issue counts)
2. **Review Findings** - From all agents (Critical, Quality, Security, Convention, Best Practices)
3. **Verification Findings** - From code-researcher (API mismatches or documentation issues)
4. **Test Results** - From code-test (Test coverage, failures, and fixes)
5. **Lint Results** - From code-lint (Auto-fixed summary and manual fix requirements)
6. **Action items** - Prioritized list of fixes (Critical > High > Low)
