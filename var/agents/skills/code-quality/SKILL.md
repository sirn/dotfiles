---
name: code-quality
description: Run comprehensive quality checks by orchestrating review, verification, testing, and linting.
---

Run comprehensive quality checks by orchestrating sub-skills.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If $ARGUMENTS provided, focus on those specific files/paths

2. Run all four skills in parallel using the Skill tool:
   - Invoke 'code-review' skill (Full mode)
   - Invoke 'code-verify' skill (Verify API usage against docs)
   - Invoke 'code-test' skill (Run tests)
   - Invoke 'code-lint' skill (Run linting)

3. Wait for all tasks to complete
4. Consolidate findings into a single report

## Output
1. **Summary** - Overall code health assessment (including issue counts)
2. **Review Findings** - From code-review (Critical, Quality, Security, Convention, Best Practices)
3. **Verification Findings** - From code-verify (API mismatches or documentation issues)
4. **Test Results** - From code-test (Test coverage, failures, and fixes)
5. **Lint Results** - From code-lint (Auto-fixed summary and manual fix requirements)
6. **Action items** - Prioritized list of fixes (Critical > High > Low)