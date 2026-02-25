---
name: code-generate-tests
description: Generate tests for untested functions and edge cases, then run them. Use when user asks to generate tests, add tests, or create test coverage.
---

Generate tests for untested functions and edge cases, then verify they pass.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those
   - Understand which functions/modules need test coverage

2. Detect test framework:
   - Invoke `code-analyze-project` skill to detect test framework and testing patterns
   - Check for existing test files and naming conventions
   - Identify test helper functions and fixtures in use

3. Identify untested code paths:
   - Use Grep to find functions without corresponding tests
   - Check for edge cases, error paths, boundary conditions
   - Identify critical paths that lack coverage

4. Research and analyze:
   - Research testing best practices for the language/framework (use WebSearch/WebFetch)
   - Identify critical paths, edge cases, and error handling scenarios

5. Generate test code:
   - Match existing test conventions (naming, structure, fixtures)
   - Cover happy path, edge cases, and error scenarios
   - Include proper assertions and test organization

6. Run the generated tests:
   - Invoke `test` skill to run the new tests
   - Analyze any failures

7. Fix test failures:
   - Identify root cause of failures
   - Fix test code or generated code as appropriate
   - Re-run tests to verify

## Stop Condition
- If test generation or fixing fails twice, stop and ask for guidance.

## Output
1. **Test Framework Detected**
2. **Untested Functions/Paths Identified**
3. **Generated Tests** (with file locations and coverage summary)
4. **Test Results** (from running tests)
5. **Failures Fixed** (if any, with explanations)
6. **Verification** - All new tests pass
