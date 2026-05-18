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
   - `scout`: "Analyze existing schema definitions, migrations, models, query code, tests, and persistence conventions for {task}."
   - `researcher`: "Research official database/ORM documentation, migration constraints, performance considerations, and security guidance relevant to {task}."
   - `planner`: "Design a minimal schema plan for {task}, including relationships, indexes, constraints, migrations, alternatives, and tradeoffs."
   - `reviewer`: "Review the proposed schema for correctness, data integrity, security, performance, simplicity, and project-convention risks."

3. Use `oracle` only for high-impact or conflicting schema decisions:
   - `oracle`: "Adjudicate the conflicting schema recommendations for {task}. Choose the safest minimal data model and state assumptions, tradeoffs, and confidence."

4. Read relevant code yourself and validate agent findings.

5. Design the schema:
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
