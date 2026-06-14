---
name: code-plan-implementation
description: Generate implementation plans. Use when asked to plan how to implement a feature, fix, integration, or code change before modifying files.
---

Generate an actionable implementation plan based on task analysis and research.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files or paths, focus on those.
- Understand the user's requested behavior, constraints, and expected verification.

### Step 2 - Research and Scout

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

For large or cross-module projects, spawn `architect` subagent before `planner` subagent:

```
Analyze module boundaries, ownership, data flow, dependency direction, and structural invariants for {task}. Recommend the minimal architecture that solves the problem.
```

### Step 4 - Adjudicate Decisions

Use `oracle` subagent only for high-impact or conflicting design decisions:

```
Adjudicate the conflicting recommendations for {task}. Choose the safest minimal path and state assumptions, tradeoffs, and confidence.
```

### Step 5 - Audit

For production-bound changes, spawn `auditor` subagent after the plan is synthesized:

```
Audit the implementation plan for {task} for production risks: correctness, security, data loss, migration hazards, and rollback safety.
```

### Step 6 - Validate Findings

Read relevant code yourself and validate agent findings.

### Step 7 - Design Implementation

- Define the minimal design approach, incorporating architect and auditor findings when present.
- Identify files to modify and integration points.
- Plan verification aligned with project tooling.
- Prefer no-code alternatives, boring solutions, minimal scope, and avoiding premature abstractions.

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
