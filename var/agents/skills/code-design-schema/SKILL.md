---
name: code-design-schema
description: Design database schemas, ORM models, and migrations. Use when asked to design data structures or modify the database.
---

Design data models, database schemas, and ORM definitions.

## Process
1. Identify context:
   - Identify the database technology (Postgres, MySQL, SQLite, Mongo, etc.)
   - Identify the ORM/Framework (Prisma, TypeORM, SQLAlchemy, Django, etc.)
   - Understand the entity relationships (1:1, 1:N, M:N)

2. Spawn agents:
   - `code-architect`: "Design the schema/entity relationship for {requirements} considering scalability and normalization"
   - `code-researcher`: "Lookup {framework} best practices for defining models and relationships"

3. Execute based on goal:
   - **New Schema**: Generate SQL DDL or ORM model definitions
   - **Modification**: Analyze current schema vs desired state, generate migration steps
   - **Validation**: Check for normalization issues or N+1 query risks in relationships

## Output
1. **Schema Design** (ER Diagram description or Model code)
2. **Migration Plan** (SQL or CLI commands)
3. **Rationale** (Why these types/indices were chosen)
