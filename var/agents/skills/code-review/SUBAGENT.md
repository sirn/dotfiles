---
name: code-review
description: Comprehensive code review with support for fast, full, or performance-focused modes.
---

Run a code review using specialized agents.

## Modes
- **Full** (default): Uses all 5 agents (Quality, Security, Convention, Simplicity, Tech Researcher)
- **Fast**: Uses 2 agents (Quality, Simplicity) for quick feedback
- **Performance**: Uses Tech Researcher to focus specifically on performance optimization

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those

2. Determine mode based on user request (default to **Full**)

3. Spawn agents in parallel using the Task tool:

   **Full Mode**:
   - `quality-reviewer`: "Review {files} for bugs, logic errors, and error handling issues"
   - `security-researcher`: "Review {files} for security vulnerabilities and OWASP risks"
   - `convention-reviewer`: "Review {files} for naming and code organization consistency"
   - `simplicity-reviewer`: "Review {files} for over-engineering and unnecessary complexity"
   - `code-researcher`: "Research best practices and patterns for libraries used in {files}"

   **Fast Mode**:
   - `quality-reviewer`: "Review {files} for bugs, logic errors, and error handling issues"
   - `simplicity-reviewer`: "Review {files} for over-engineering and unnecessary complexity"

   **Performance Mode**:
   - `code-researcher`: "Analyze {files} for performance bottlenecks, algorithmic complexity, and memory usage. Suggest concrete optimizations."

4. Synthesize findings into a unified report

## Output
1. **Executive Summary**
2. **Critical Issues** (Must fix)
3. **Security Analysis** (Full mode only)
4. **Quality & Logic**
5. **Simplicity & Convention**
6. **Performance & Best Practices**
7. **Quick Wins**
