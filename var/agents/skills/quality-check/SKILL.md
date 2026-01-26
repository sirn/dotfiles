---
name: quality-check
description: Run comprehensive quality checks by orchestrating review, performance, testing, and linting skills. Use when user asks to check quality or run comprehensive analysis.
---

Run comprehensive quality checks by orchestrating sub-skills.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If $ARGUMENTS provided, focus on those specific files/paths

2. Run all four skills in parallel using the Skill tool:
   - Invoke 'review' skill (spawns 5 parallel reviewer agents)
   - Invoke 'performance-review' skill (analyzes performance)
   - Invoke 'test' skill (runs tests)
   - Invoke 'lint' skill (runs linting tools)

3. Wait for all four tasks to complete
4. Consolidate findings into a single report

## Output
1. **Summary** - Overall code health assessment (including issue counts)
2. **Review Findings** - From review (Critical, Quality, Convention, Best Practices)
3. **Security Findings** - From review (Vulnerabilities, threat modeling)
4. **Performance Issues** - From performance-review (Problem descriptions and optimized solutions)
5. **Test Results** - From test (Test coverage, failures, and fixes)
6. **Lint Results** - From lint (Auto-fixed summary and manual fix requirements)
7. **Action items** - Prioritize list of fixes (Critical > High > Low)
