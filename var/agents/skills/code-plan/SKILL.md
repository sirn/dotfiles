---
name: code-plan
description: Generate comprehensive implementation plan based on analysis. Use when user asks to plan this, create a plan, how should I implement, or wants implementation guidance.
---

Generate a comprehensive implementation plan based on task analysis.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those
   - Understand the user's task/request

2. Analyze the codebase:
   - Read relevant code areas and identify existing patterns
   - Understand existing architectural decisions
   - Identify integration points with current codebase

3. Research and plan:
   - Research best practices and official documentation (use WebSearch/WebFetch)
   - Identify security risks and secure implementation patterns
   - Consider simplicity constraints and avoid over-engineering
   - Identify recommended libraries/tools with rationale

4. Synthesize findings into a clear implementation plan

## Output
1. **Context Analysis**
   - Relevant code structure and patterns
   - Existing architectural decisions
   - Integration points with current codebase

2. **Security Considerations**
   - Threat modeling and potential attack vectors
   - Authentication and authorization requirements
   - Data protection and privacy considerations
   - Secure implementation patterns

3. **Documentation & Best Practices**
   - Relevant docs or API constraints
   - Industry standards with authoritative sources
   - Recommended libraries/tools with rationale
   - Common pitfalls to avoid

4. **Simplicity Constraint**
   - "Keep it simple" guidelines
   - Over-engineering risk to avoid
   - Pragmatic vs ideal tradeoffs

5. **Architectural Guidance**
   - High-level design approach
   - Module boundaries and interfaces
   - Design tradeoffs considered

6. **Implementation Plan**
   - Numbered, concrete steps
   - File to modify with specific locations
   - Dependencies to add (research-backed)
   - Testing strategy aligned with project patterns

Prioritize actionable, specific guidance over abstract advice.
