---
name: code-plan-implementation
description: Generate implementation plans. Use when asked to plan how to implement a feature, fix, integration, or code change before modifying files.
---

Generate an actionable implementation plan based on task analysis and research.

## Process

### Step 1 - Identify Context

- For existing code changes, run `jj diff -s` to list changed files, and `jj diff -- path` to inspect specific files or directories.
- Focus on any user-specified files or paths.
- Understand the expected behavior, constraints, and verification requirements.

### Step 2 - Research and Scout

- When the task introduces new modules, interfaces, or abstractions, read the `software-design` reference and evaluate at least two design alternatives against its principles (deep modules, information hiding, pull complexity downward, design-it-twice).
- When structuring business logic, data access, or persistence layers, read the `eaa-patterns` reference and choose appropriate domain-logic (Transaction Script / Domain Model / Table Module / Service Layer), data-source (Active Record vs Data Mapper), and O/R mapping patterns rather than inventing ad-hoc structure.

Spawn `scout` subagent:

```
Analyze affected code areas, existing patterns, tests, architecture, integration points, and local conventions for {task}.
```

Spawn `researcher` subagent:

```
Research official documentation, best practices, constraints, migration considerations, and security guidance relevant to {task}.
```

Spawn `planner` subagent:

```
Design a minimal implementation plan for {task}, including alternatives and tradeoffs.
```

Spawn `reviewer` subagent:

```
Review the proposed direction for security, correctness, simplicity, and project-convention risks for {task}.
```

### Step 3 - Architecture Review

For large or cross-module tasks, spawn the `architect` subagent before the `planner` subagent:

```
Analyze module boundaries, ownership, data flow, dependency direction, and structural invariants for {task}. Recommend the minimal architecture that solves the problem.
```

### Step 4 - Adjudicate Decisions

Use the `oracle` subagent to resolve high-impact or conflicting design decisions:

```
Adjudicate the conflicting recommendations for {task}. Choose the safest minimal path and state assumptions, tradeoffs, and confidence.
```

### Step 5 - Audit

For production-bound changes, spawn the `auditor` subagent after drafting the plan:

```
Audit the implementation plan for {task} for production risks: correctness, security, data loss, migration hazards, and rollback safety.
```

### Step 6 - Validate Findings

Read the relevant code directly to validate all agent findings.

### Step 7 - Design Implementation

- Define a minimal approach, incorporating `architect` and `auditor` findings when present.
- Identify target files and integration points.
- Align verification with project-specific tooling.
- Prioritize simple, no-code, or boring solutions; avoid premature abstractions; favor fewer, deeper modules; avoid speculative generality.

### Step 8 - Report

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

6. **Implementation Plan**
   - Numbered concrete steps
   - File targets
   - Verification strategy

Prioritize actionable, specific guidance over abstract advice.
