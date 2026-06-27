---
name: code-explain
description: Explain code, triage changes, or map project structure. Use when user asks to explain, understand, triage, or explore project structure.
---

Explain code logic, triage incoming changes, or map the project architecture.

## Process

### Step 1 - Identify Context

- If code changes are involved, run `jj diff -s` first to view changed files.
- If specific files or paths are specified, focus on those.

### Step 2 - Determine Goal

- **Explain**: To understand existing code.
- **Triage**: To assess and understand a code diff.
- **Map**: To get a high-level overview of the project structure.

### Step 3 - Execute Based on Goal

#### Explain

Spawn `scout` subagent:

```
Identify the purpose, local patterns, data flow, and integration points in:
{files}
```

Spawn `researcher` subagent if external library context is needed:

```
Look up documentation for libraries/frameworks used in:
{files}

Focus only on APIs needed to understand the code.
```

Spawn `architect` subagent if the user asks for architecture or tradeoff explanations:

```
Explain the architectural choices and tradeoffs visible in:
{files}
```

**Synthesize**: Purpose, how it works, patterns, dependencies, and gotchas.

#### Triage

Skim relevant diffs, and spawn `scout` subagent if they touch unfamiliar areas:

```
Map the files, ownership boundaries, and neighboring tests for this diff.

Identify:
- areas touched
- risk hotspots
- review order
```

#### Map

Spawn `scout` subagent:

```
Create a high-level dependency graph and module breakdown for:
{directory}

Identify:
- entry points
- core domain logic
- infrastructure/adapters
```

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
