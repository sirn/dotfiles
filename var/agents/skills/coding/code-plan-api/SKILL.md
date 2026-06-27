---
name: code-plan-api
description: Design API contracts. Use when asked to plan REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, or schema contract changes.
---

Generate an actionable API design plan based on task analysis and research.

## Process

### Step 1 - Identify Context

- For code changes, run `jj diff -s` to view changed files, and `jj diff -- path` to restrict focus to specific files/directories.
- Focus on user-specified files, endpoints, clients, protocols, or paths.
- Identify the target protocol and schema format (REST, GraphQL, gRPC, TRPC, OpenAPI, protobuf, Zod/TypeBox, etc.).

### Step 2 - Research and Scout

Spawn `scout` subagent:

```
Analyze existing API handlers, clients, schemas, validators, middleware, tests, and local API conventions for {task}.
```

Spawn `researcher` subagent:

```
Research official protocol/framework documentation, constraints, security guidance, and compatibility considerations relevant to {task}.
```

Spawn `planner` subagent:

```
Design a minimal API contract for {task}, including request/response shapes, errors, validation, and tradeoffs.
```

Spawn `reviewer` subagent:

```
Review the proposed API for correctness, security, compatibility, simplicity, and project-convention risks.
```

### Step 3 - Architecture Review

For APIs spanning module or service boundaries, spawn the `architect` subagent before the `planner`:

Spawn `architect` subagent:

```
Analyze module boundaries, ownership, data flow, and interface contracts for {task}. Recommend the minimal API boundary that solves the problem.
```

### Step 4 - Adjudicate Decisions

For high-impact or conflicting design choices, spawn the `oracle` subagent:

```
Adjudicate the conflicting API design recommendations for {task}. Choose the safest minimal contract and state assumptions, tradeoffs, and confidence.
```

### Step 5 - Audit

For production-bound changes, spawn the `auditor` subagent after synthesizing the plan:

```
Audit the API design for {task} for production risks: contract compatibility, breaking changes, data loss, and rollback safety.
```

### Step 6 - Validate Findings

Read relevant code directly to validate subagent findings.

### Step 7 - Design API

- Design only required resources, operations, fields, and data shapes.
- Prefer flat, simple structures over deeply nested designs.
- Consider versioning, pagination, error handling, auth, validation, and compatibility.
- Identify security risks and secure API patterns.

### Step 8 - Report

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
