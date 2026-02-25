---
name: code-review
description: Comprehensive code review with support for fast, full, or performance-focused modes.
---

Run a code review.

## Modes
- **Full** (default): Review for quality, security, conventions, simplicity, and best practices
- **Fast**: Review for quality and simplicity only
- **Performance**: Focus on performance optimization

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those

2. Determine mode based on user request (default to **Full**)

3. Review the code:

   **Full Mode** — analyze each aspect:
   - **Quality**: Check for bugs, logic errors, error handling issues, and edge cases
   - **Security**: Check for OWASP risks, injection vulnerabilities, auth issues, data exposure
   - **Conventions**: Check naming consistency, code organization, and style adherence
   - **Simplicity**: Check for over-engineering, unnecessary abstractions, and dead code
   - **Best practices**: Research patterns and idioms for the libraries/frameworks in use (use WebSearch/WebFetch as needed)

   **Fast Mode**:
   - **Quality**: Check for bugs, logic errors, and error handling issues
   - **Simplicity**: Check for over-engineering and unnecessary complexity

   **Performance Mode**:
   - Analyze for performance bottlenecks, algorithmic complexity, and memory usage
   - Suggest concrete optimizations with benchmarks where possible

4. Synthesize findings into a unified report

## Output
1. **Executive Summary**
2. **Critical Issues** (Must fix)
3. **Security Analysis** (Full mode only)
4. **Quality & Logic**
5. **Simplicity & Convention**
6. **Performance & Best Practices**
7. **Quick Wins**
