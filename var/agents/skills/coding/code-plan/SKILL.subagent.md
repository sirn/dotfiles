---
name: code-plan
description: Generate implementation, API, schema, or refactoring plans using specialized subagents when available. Use when asked to plan, design APIs or database schemas, or create a safe refactoring plan.
---

Generate a comprehensive plan based on task analysis and research.

## Modes

- **Implementation** (default): General implementation plan.
- **API**: REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, or schema contract design.
- **Schema**: Database schemas, ORM models, migrations, and data relationships.
- **Refactor**: Safe targeted refactoring plan only; do not apply changes.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.
   - Understand the user's task/request and determine mode.

2. Spawn applicable agents in parallel:
   - `scout`: "Analyze affected code areas, existing patterns, tests, architecture, integration points, and local conventions for {task}."
   - `researcher`: "Research official documentation, best practices, constraints, migration considerations, and security guidance relevant to {task}."
   - `planner`: "Design a minimal implementation/refactoring/API/schema plan for {task}, including alternatives and tradeoffs."
   - `reviewer`: "Review the proposed direction for security, correctness, simplicity, and project-convention risks for {task}."

3. Use `oracle` only for high-impact or conflicting design decisions:
   - `oracle`: "Adjudicate the conflicting recommendations for {task}. Choose the safest minimal path and state assumptions, tradeoffs, and confidence."

4. Read relevant code yourself and validate agent findings.

5. Execute by mode:

   **Implementation**:
   - Define the minimal design approach.
   - Identify files to modify and integration points.
   - Plan verification aligned with project tooling.

   **API**:
   - Identify protocol and format: REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, Zod/TypeBox, etc.
   - Design only required resources, operations, fields, and data shapes.
   - Prefer flat, simple structures over deeply nested designs.
   - Consider versioning, pagination, error handling, auth, validation, and compatibility.
   - Provide example requests/responses.

   **Schema**:
   - Identify database and ORM/framework.
   - Understand relationships: 1:1, 1:N, M:N.
   - Define only required tables/entities/fields.
   - Avoid generic/flexible schemas unless strictly required.
   - Consider indexes, constraints, migrations, normalization, and N+1 risks.

   **Refactor**:
   - Identify safe transformations: extraction, rename, simplification, deletion, inlining, or module movement.
   - Audit abstractions: does each abstraction earn its complexity?
   - Prefer deletion and simplification over new layers.
   - Provide step-by-step order and verification after each meaningful step.
   - Do not apply code changes.

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

6. **Mode-Specific Artifact**
   - API specification, schema design, refactoring plan, or implementation outline

7. **Implementation Plan**
   - Numbered concrete steps
   - File targets
   - Verification strategy

Prioritize actionable, specific guidance over abstract advice.
