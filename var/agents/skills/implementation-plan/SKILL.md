---
name: implementation-plan
description: Generate comprehensive implementation plan based on analysis. Use when user asks to plan this, create a plan, how should I implement, or wants implementation guidance.
---

Generate a comprehensive implementation plan based on task analysis and research.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If $ARGUMENTS provided, focus on those specific files/paths
   - Understand the user's task/request

2. Spawn all five agents in parallel using the Task tool:
   - Use `code-architect` agent: Analyze affected code areas and existing patterns
   - Use `security-researcher` agent: Identify security risks and recommend secure patterns
   - Use `best-practices-researcher` agent: Research best practices for the task
   - Use `simplicity-reviewer` agent: Identify over-engineering risks and pragmatic constraints
   - Use `doc-researcher` agent: Look up relevant documentation and constraints
3. Synthesize all findings into a clear implementation plan

## Output
1. **Context Analysis** (from code-architect)
   - Relevant code structure and patterns
   - Existing architectural decisions
   - Integration points with current codebase

2. **Security Considerations** (from security-researcher)
   - Threat modeling and potential attack vectors
   - Authentication and authorization requirements
   - Data protection and privacy considerations
   - Secure implementation patterns

3. **Documentation** (from doc-researcher)
   - Relevant docs or API constraints
   - Required configuration or version notes

4. **Best Practices** (from best-practices-researcher)
   - Industry standards with authoritative sources
   - Recommended libraries/tools with rationale
   - Common pitfalls to avoid

5. **Simplicity Constraint** (from simplicity-reviewer)
   - "Keep it simple" guidelines
   - Over-engineering risk to avoid
   - Pragmatic vs ideal tradeoffs

6. **Architectural Guidance** (from code-architect)
   - High-level design approach
   - Module boundaries and interfaces
   - Design tradeoffs considered

7. **Implementation Plan** (synthesize)
   - Numbered, concrete steps
   - File to modify with specific locations
   - Dependencies to add (research-backed)
   - Testing strategy aligned with project patterns

Prioritize actionable, specific guidance over abstract advice.
