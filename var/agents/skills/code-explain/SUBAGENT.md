---
name: code-explain
description: Explain code, triage changes, or map project structure.
---

Explain code logic, triage incoming changes, or map the project architecture.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files
   - If the user specified specific files or paths, focus on those

2. Determine goal:
   - **Explain**: User wants to understand existing code
   - **Triage**: User wants to understand/assess a diff
   - **Map**: User wants a high-level overview of the project structure

3. Execute based on goal:

   **Explain**:
   - Spawn `code-researcher` and `code-architect` agents in parallel
   - `code-researcher`: "Lookup documentation for libraries/frameworks used in {files}"
   - `code-architect`: "Identify patterns and data flow in {files}"
   - Synthesize: Purpose, How it works, Patterns, Dependencies, Gotchas

   **Triage**:
   - Skim relevant diffs
   - Identify: Areas touched, Risk hotspots, Review order

   **Map**:
   - Spawn `code-architect` to analyze the directory structure and imports
   - "Create a high-level dependency graph and module breakdown for {directory}"
   - Identify: Key Entry Points, Core Domain Logic, Infrastructure/Adapters

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
1. **High-Level Diagram** (Mermaid or Text tree)
2. **Key Modules & Responsibilities**
3. **Data Flow Overview**
