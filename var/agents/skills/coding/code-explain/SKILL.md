---
name: code-explain
description: Explain code, triage changes, or map project structure. Use when user asks to explain, understand, triage, or explore project structure.
---

Explain code logic, triage incoming changes, or map project architecture.

## Process

### Step 1 - Identify Context

- For code changes, `jj diff -s` to view changed files.
- Focus on specified files or paths.

### Step 2 - Determine Goal

- **Explain**: understand existing code.
- **Triage**: assess and understand a code diff.
- **Map**: high-level overview of project structure.

### Step 3 - Execute Based on Goal

When applying a scout lens below:

- Map relevant files, conventions, and call paths; cite file paths and line numbers.
- Distinguish confirmed patterns from one-offs; stay read-only and task-relevant.

#### Explain

- Apply a scout lens to identify the purpose, local patterns, data flow, and integration points in {files}.
- If external library context is needed, apply a researcher lens to look up docs for libraries/frameworks in {files}, focusing only on APIs needed to understand the code:
  - Prefer official documentation over blog posts; cite sources with URLs.
  - Separate confirmed facts from plausible interpretations; note version requirements.
- If the user asks for architecture or tradeoff explanations, apply an architect lens to explain the architectural choices and tradeoffs visible in {files}:
  - Map current module boundaries, ownership, and dependency direction first.
  - Recommend the smallest architecture that preserves invariants.
  - Avoid speculative generality.

**Synthesize**: purpose, how it works, patterns, dependencies, and gotchas.

#### Triage

Skim relevant diffs.

- If they touch unfamiliar areas, apply a scout lens to map the files, ownership boundaries, and neighboring tests for this diff, identifying areas touched, risk hotspots, and the review order.

#### Map

- Apply a scout lens to create a high-level dependency graph and module breakdown for {directory}, identifying entry points, core domain logic, and infrastructure/adapters.

### Step 4 - Report

#### Explanation

1. **Purpose & Mechanics**
2. **Patterns & Dependencies**
3. **Gotchas**

#### Triage

1. **Change Summary**
2. **Risk Hotspots**
3. **Suggested Review Order**

#### Map

1. **High-Level Diagram** (Mermaid or text tree)
2. **Key Modules & Responsibilities**
3. **Data Flow Overview**
