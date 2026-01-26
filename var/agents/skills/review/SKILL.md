---
name: review
description: Review for issues and improvements using specialized agents. Use when user asks for review or feedback on changes.
---

Run a comprehensive review using specialized reviewer agents.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If $ARGUMENTS provided, focus on those specific files/paths

2. Spawn all five reviewer agents in parallel using the Task tool:
    - Use `quality-reviewer` agent: Focuses on bugs and logic errors
    - Use `security-researcher` agent: Conducts deep security analysis and threat modeling
    - Use `convention-reviewer` agent: Checks naming, organization, and consistency
    - Use `simplicity-reviewer` agent: Identifies over-engineering and complexity
    - Use `best-practices-researcher` agent: Researches best practices for patterns/libraries used

3. Each agent should review the changed files or specified files ($ARGUMENTS)

4. Collect and synthesize their findings into a unified report

## Agent Invocation
Use the Task tool with these exact subagent_types:
- `quality-reviewer`: "Review {files} for bugs, logic errors, and error handling issues"
- `security-researcher`: "Review {files} for security vulnerabilities and OWASP risks"
- `convention-reviewer`: "Review {files} for naming and code organization consistency"
- `simplicity-reviewer`: "Review {files} for over-engineering and unnecessary complexity"
- `best-practices-researcher`: "Research best practices for libraries/patterns used in {files}"

Each agent should return findings with severity (Critical/High/Medium/Low) and file:line references.

## Output
Present a consolidated review with:
1. **Executive Summary** (3-5 bullet points of most important findings)
2. **Critical Issues** (must fix before merge)
3. **Security Analysis** (from security-researcher)
4. **Quality Issues** (from quality-reviewer)
5. **Convention Issues** (from convention-reviewer)
6. **Simplification Opportunities** (from simplicity-reviewer)
7. **Best Practices** (from best-practices-researcher, with source links)
8. **Quick Wins** (easy fixes with high impact)

Deduplicate overlapping findings and prioritize by severity.
