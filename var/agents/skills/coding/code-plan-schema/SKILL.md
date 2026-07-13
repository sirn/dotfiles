---
name: code-plan-schema
description: Design database schemas, ORM models, migrations, and data relationships. Use when asked to plan schema or persistence changes.
---

Generate an actionable schema design plan based on task analysis and research.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- Focus on user-specified entities, tables, models, migrations, or relationships.
- Identify the database, ORM/framework, migration tooling, and existing data model.

### Step 2 - Research and Scout

Spawn `scout` subagent:

```
Analyze existing schema artifacts for {task}:
- Schema definitions
- Migrations
- Models
- Query code
- Tests
- Persistence conventions
```

Spawn `researcher` subagent:

```
Research official database/ORM documentation relevant to {task}:
- Migration constraints
- Performance considerations
- Security guidance
```

Spawn `planner` subagent:

```
Design a minimal schema plan for {task}, including:
- Relationships
- Indexes
- Constraints
- Migrations
- Alternatives
- Tradeoffs
```

Spawn `reviewer` subagent:

```
Review the proposed schema for:
- Correctness
- Data integrity
- Security
- Performance
- Simplicity
- Project-convention risks
```

### Step 3 - Architecture Review

For schemas affecting data flow or cross-module boundaries, spawn `architect` subagent before `planner` subagent:

```
Analyze for {task}:
- Data flow
- Ownership
- Dependency direction
- Migration shape
Recommend the minimal schema that preserves invariants.
```

### Step 4 - Adjudicate Decisions

Use `oracle` subagent only for high-impact or conflicting schema decisions:

```
Adjudicate the conflicting schema recommendations for {task}.
Choose the safest minimal data model and state:
- Assumptions
- Tradeoffs
- Confidence
```

### Step 5 - Audit

For production-bound schema changes, spawn `auditor` subagent after the plan is synthesized:

```
Audit the schema plan for {task} for production risks:
- Data loss
- Migration hazards
- Rollback safety
- Contract compatibility
```

### Step 6 - Validate Findings

Read relevant code yourself and validate agent findings.

### Step 7 - Design Schema

- Define only required tables, entities, and fields.
- Avoid generic or over-flexible schemas.
- Account for indexes, constraints, migrations, normalization, and N+1 query risks.
- When mapping objects to tables or modeling relationships, read the `eaa-patterns` reference and apply its O/R structural and metadata patterns: choose Single/Class/Concrete Table Inheritance for hierarchies, Association Table Mapping for many-to-many, Embedded Value for small owned objects, and Identity Field for object identity; consider Lazy Load and Identity Map for N+1 and consistency.
- Identify security and privacy risks for stored data.

### Step 8 - Report

1. **Context Analysis**
   - Existing schema/model structure
   - Query patterns and integration points

2. **Security Considerations**
   - Data exposure, tenant boundaries, privacy, and integrity risks

3. **Documentation & Best Practices**
   - Relevant database/ORM constraints
   - Common migration pitfalls

4. **Simplicity Constraint**
   - Minimal required entities and fields
   - Over-engineering risks

5. **Schema Design**
   - Tables/entities/fields
   - Relationships
   - Indexes and constraints
   - Migration/backfill considerations

6. **Implementation Plan**
   - Numbered concrete steps
   - File targets
   - Verification strategy

Prioritize actionable, specific guidance over abstract advice.
