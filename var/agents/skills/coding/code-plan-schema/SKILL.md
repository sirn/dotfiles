---
name: code-plan-schema
description: Design database schemas, ORM models, migrations, and data relationships. Use when asked to plan schema or persistence changes.
---

Generate an actionable schema design plan based on task analysis.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified entities, tables, models, migrations, or relationships, focus on those.
   - Identify the database, ORM/framework, migration tooling, and existing data model.

2. Analyze the codebase:
   - Read existing schema definitions, migrations, models, query code, and tests.
   - Understand relationships: 1:1, 1:N, M:N.
   - Identify integration points, migration constraints, and data compatibility risks.

3. Research and design:
   - Research official database/ORM documentation when needed.
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
