---
name: code-explain
description: Explain code, triage changes, or map project structure. Use when user asks to explain, understand, triage, or explore project structure.
---

Explain code logic, triage incoming changes, or map the project architecture.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files.
- If the user specified specific files or paths, focus on those.

### Step 2 - Determine Goal

- **Explain**: User wants to understand existing code.
- **Triage**: User wants to understand/assess a diff.
- **Map**: User wants a high-level overview of the project structure.

### Step 3 - Execute Based on Goal

#### Explain

Spawn `scout` subagent:

```
Identify the purpose, local patterns, data flow, and integration points in:
{files}
```

Spawn `researcher` subagent when external library context matters.

```
Look up documentation for libraries/frameworks used in:
{files}

Focus only on APIs needed to understand the code.
```

Spawn `architect` subagent only when the user asks for architecture/tradeoff explanation:

```
Explain the architectural choices and tradeoffs visible in:
{files}
```

Spawn `architect` subagent only for architecture/tradeoff commentary when requested.

**Synthesize**: Purpose, How it works, Patterns, Dependencies, Gotchas.

#### Triage

Skim relevant diffs and spawn `scout` subagent if the diff touches unfamiliar areas:

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

#### For Triage

1. **Change Summary**
2. **Risk Hotspots**
3. **Suggested Review Order**

#### For Map

1. **High-Level Diagram** (Mermaid or text tree)
2. **Key Modules & Responsibilities**
3. **Data Flow Overview**
