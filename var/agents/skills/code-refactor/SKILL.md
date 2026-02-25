---
name: code-refactor
description: Execute safe, targeted refactoring with step-by-step guidance. Use when user asks to refactor, extract, rename, or restructure code.
---

Execute safe, targeted refactoring by analyzing code and providing actionable steps.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those
   - Understand the refactoring goal (extract function, rename, simplify, etc.)

2. Analyze the code:
   - Identify safe refactoring transformations and structural changes
   - Identify over-engineered areas to simplify, dead code to remove
   - Research idiomatic refactoring patterns for the language (use WebSearch/WebFetch)

3. Synthesize findings into concrete refactoring steps

## Output
Present a refactoring plan with:
1. **Identified Refactorings** - Each refactoring opportunity with rationale
   - Function extraction
   - Variable renaming
   - Dead code removal
   - Complexity reduction

2. **Complexity Analysis**
   - Over-engineered areas
   - Unnecessary abstractions
   - Dead code identified

3. **Best Practices Alignment**
   - Idiomatic patterns to apply
   - Language-specific refactorings
   - Modern alternatives to legacy code

4. **Step-by-Step Plan** - Numbered, file:line specific
   - Each step with purpose and expected outcome
   - Safe ordering (dependencies first)

5. **Verification Steps** - How to confirm each refactoring works
   - Run tests after each major refactoring
   - Commands to validate behavior

IMPORTANT: Only provide the plan. Do NOT auto-apply changes.
