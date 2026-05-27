---
name: code-plan-api
description: Design API contracts. Use when asked to plan REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, or schema contract changes.
---

Generate an actionable API design plan based on task analysis and research.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, endpoints, clients, protocols, or paths, focus on those.
   - Identify the protocol and format: REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, Zod/TypeBox, etc.

2. Spawn applicable agents in parallel:
   - `scout`: "Analyze existing API handlers, clients, schemas, validators, middleware, tests, and local API conventions for {task}."
   - `researcher`: "Research official protocol/framework documentation, constraints, security guidance, and compatibility considerations relevant to {task}."
   - `planner`: "Design a minimal API contract for {task}, including request/response shapes, errors, validation, and tradeoffs."
   - `reviewer`: "Review the proposed API for correctness, security, compatibility, simplicity, and project-convention risks."

3. For APIs spanning module or service boundaries, spawn `architect` before `planner`:
   - `architect`: "Analyze module boundaries, ownership, data flow, and interface contracts for {task}. Recommend the minimal API boundary that solves the problem."

4. Use `oracle` only for high-impact or conflicting API design decisions:
   - `oracle`: "Adjudicate the conflicting API design recommendations for {task}. Choose the safest minimal contract and state assumptions, tradeoffs, and confidence."

5. For production-bound API changes, spawn `auditor` after the plan is synthesized:
   - `auditor`: "Audit the API design for {task} for production risks: contract compatibility, breaking changes, data loss, and rollback safety."

6. Read relevant code yourself and validate agent findings.

7. Design the API:
   - Design only required resources, operations, fields, and data shapes.
   - Prefer flat, simple structures over deeply nested designs.
   - Consider versioning, pagination, error handling, auth, validation, and compatibility.
   - Identify security risks and secure API patterns.

## Output

1. **Context Analysis**
   - Existing API structure and patterns
   - Clients/consumers and integration points

2. **Security Considerations**
   - Auth, authorization, validation, data exposure, and abuse risks

3. **Documentation & Best Practices**
   - Relevant protocol/framework constraints
   - Common pitfalls

4. **Simplicity Constraint**
   - Minimal resources, operations, and fields
   - Over-engineering risks

5. **API Design**
   - Resources/operations or service methods
   - Request/response shapes
   - Errors, pagination, versioning, auth, and validation
   - Example requests/responses

6. **Implementation Plan**
   - Numbered concrete steps
   - File targets
   - Verification strategy

Prioritize actionable, specific guidance over abstract advice.
