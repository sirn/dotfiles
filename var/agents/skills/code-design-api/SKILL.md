---
name: code-design-api
description: Design API contracts and specifications. Use when asked to design REST, GraphQL, or RPC endpoints.
---

Design API contracts, schemas, and specifications before implementation.

## Process
1. Identify context:
   - Protocol: REST, GraphQL, gRPC, TRPC
   - Format: OpenAPI/Swagger, Protocol Buffers, Zod/TypeBox schemas

2. Spawn agents:
   - `code-architect`: "Design the API resources, endpoints, and data shapes for {requirements}"
   - `code-researcher`: "Lookup best practices for {protocol} API design (versioning, pagination, errors)"

3. Output the design:
   - **Endpoints/Operations**: Method, Path, Name
   - **Request Schema**: input validation rules
   - **Response Schema**: Success/Error shapes
   - **Examples**: JSON/Text payloads

## Output
1. **API Specification** (OpenAPI YAML, Proto file, or Interface code)
2. **Usage Example** (curl or client code)
3. **Design Rationale** (RESTfulness, Breaking changes avoidance)
