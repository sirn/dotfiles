---
name: code-plan-implementation
description: Generate implementation plans. Use when asked to plan how to implement a feature, fix, integration, or code change before modifying files.
---

Generate an actionable implementation plan based on task analysis.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.
   - Understand the user's requested behavior, constraints, and expected verification.

2. Analyze the codebase:
   - Read relevant code areas and identify existing patterns.
   - Understand architectural decisions, module boundaries, and integration points.
   - Identify over-engineering risks and simpler alternatives.

3. Research when needed:
   - Research official documentation and best practices when the plan depends on external APIs, framework behavior, or unfamiliar tooling.
   - Identify security risks and secure implementation patterns.
   - Recommend libraries/tools only when they are necessary and research-backed.

4. Design the implementation:
   - Define the minimal design approach.
   - Identify files to modify and integration points.
   - Plan verification aligned with project tooling.
   - Prefer no-code alternatives, boring solutions, minimal scope, and avoiding premature abstractions.

## Output

1. **Context Analysis**
   - Relevant code structure and patterns
   - Existing architectural decisions
   - Integration points

2. **Security Considerations**
   - Threats and secure patterns relevant to the plan

3. **Documentation & Best Practices**
   - Relevant docs or API constraints
   - Common pitfalls
   - Recommended libraries/tools with rationale, if any

4. **Simplicity Constraint**
   - No-code or simpler alternatives
   - Over-engineering risks
   - Minimal viable scope

5. **Design / Architecture**
   - High-level approach
   - Module boundaries and interfaces
   - Tradeoffs

6. **Implementation Plan**
   - Numbered concrete steps
   - File targets
   - Verification strategy

Prioritize actionable, specific guidance over abstract advice.
