---
name: code-plan-schema
description: Design database schemas, ORM models, migrations, and data relationships. Use when asked to plan schema or persistence changes.
---

Generate an actionable schema design plan based on task analysis and research.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified entities, tables, models, migrations, or relationships, focus on those.
   - Identify the database, ORM/framework, migration tooling, and existing data model.

2. Spawn applicable agents in parallel:
   - `scout`:
     ```
     Analyze existing schema artifacts for {task}:
     - Schema definitions
     - Migrations
     - Models
     - Query code
     - Tests
     - Persistence conventions
     ```
   - `researcher`:
     ```
     Research official database/ORM documentation relevant to {task}:
     - Migration constraints
     - Performance considerations
     - Security guidance
     ```
   - `planner`:
     ```
     Design a minimal schema plan for {task}, including:
     - Relationships
     - Indexes
     - Constraints
     - Migrations
     - Alternatives
     - Tradeoffs
     ```
   - `reviewer`:
     ```
     Review the proposed schema for:
     - Correctness
     - Data integrity
     - Security
     - Performance
     - Simplicity
     - Project-convention risks
     ```

3. For schemas affecting data flow or cross-module boundaries, spawn `architect` before `planner`:
   - `architect`:
     ```
     Analyze for {task}:
     - Data flow
     - Ownership
     - Dependency direction
     - Migration shape
     Recommend the minimal schema that preserves invariants.
     ```

4. Use `oracle` only for high-impact or conflicting schema decisions:
   - `oracle`:
     ```
     Adjudicate the conflicting schema recommendations for {task}.
     Choose the safest minimal data model and state:
     - Assumptions
     - Tradeoffs
     - Confidence
     ```

5. For production-bound schema changes, spawn `auditor` after the plan is synthesized:
   - `auditor`:
     ```
     Audit the schema plan for {task} for production risks:
     - Data loss
     - Migration hazards
     - Rollback safety
     - Contract compatibility
     ```

6. Read relevant code yourself and validate agent findings.

7. Design the schema:
   - Define only required tables/entities/fields.
   - Avoid generic/flexible schemas unless strictly required.
   - Consider indexes, constraints, migrations, normalization, and N+1 risks.
   - Identify security and privacy risks for stored data.

## Output

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
