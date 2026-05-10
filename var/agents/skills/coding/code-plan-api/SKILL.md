---
name: code-plan-api
description: Design API contracts. Use when asked to plan REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, or schema contract changes.
---

Generate an actionable API design plan based on task analysis.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, endpoints, clients, protocols, or paths, focus on those.
   - Identify the protocol and format: REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, Zod/TypeBox, etc.

2. Analyze the codebase:
   - Read relevant API handlers, clients, schemas, validators, middleware, and tests.
   - Understand existing resource naming, auth, errors, pagination, versioning, and compatibility patterns.
   - Identify integration points and migration constraints.

3. Research and design:
   - Research official protocol/framework documentation when needed.
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
