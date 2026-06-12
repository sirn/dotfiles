---
name: code-explain
description: Explain code, triage changes, or map project structure. Use when user asks to explain, understand, triage, or explore project structure.
---

Explain code logic, triage incoming changes, or map the project architecture.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files.
   - If the user specified specific files or paths, focus on those.

2. Determine goal:
   - **Explain**: User wants to understand existing code.
   - **Triage**: User wants to understand/assess a diff.
   - **Map**: User wants a high-level overview of the project structure.

3. Execute based on goal:

   **Explain**:
   - Spawn `scout` and `researcher` in parallel when external library context matters.
   - `scout`:
     ```
     Identify the purpose, local patterns, data flow, and integration points in:
     {files}
     ```
   - `researcher`:
     ```
     Look up documentation for libraries/frameworks used in:
     {files}

     Focus only on APIs needed to understand the code.
     ```
   - Use `planner` only when the user asks for architecture/tradeoff explanation:
     ```
     Explain the architectural choices and tradeoffs visible in:
     {files}
     ```
   - Synthesize: Purpose, How it works, Patterns, Dependencies, Gotchas.

   **Triage**:
   - Skim relevant diffs.
   - Spawn `scout` if the diff touches unfamiliar areas:
     ```
     Map the files, ownership boundaries, and neighboring tests for this diff.
     ```
   - Identify: Areas touched, Risk hotspots, Review order.

   **Map**:
   - Spawn `scout`:
     ```
     Create a high-level dependency graph and module breakdown for:
     {directory}

     Identify:
     - entry points
     - core domain logic
     - infrastructure/adapters
     ```
   - Use `planner` only for architecture/tradeoff commentary when requested.
   - Identify: Key Entry Points, Core Domain Logic, Infrastructure/Adapters.

## Output

**For Explanation**:

1. **Purpose & Mechanics**
2. **Patterns & Dependencies**
3. **Gotchas**

**For Triage**:

1. **Change Summary**
2. **Risk Hotspots**
3. **Suggested Review Order**

**For Map**:

1. **High-Level Diagram** (Mermaid or text tree)
2. **Key Modules & Responsibilities**
3. **Data Flow Overview**
