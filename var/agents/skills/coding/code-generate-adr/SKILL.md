---
name: code-generate-adr
description: Create an Architecture Decision Record. Use when asked to write or draft an ADR for an architectural or technical decision.
---

Create ADRs that match existing project conventions.

## Process

### Step 1 - Identify Context

- If code changes are involved, run `jj diff -s` to view changed files and `jj diff -- path` to restrict to specific files or directories.
- Focus on any user-specified decisions, alternatives, constraints, or consequences.
- Ask for missing Context, Decision, or Consequences if necessary.

### Step 2 - Research and Plan

#### Standard Research

Spawn `scout` subagent:
```
Identify existing ADR style, naming, structure, status values, and relevant architectural context for {files or requirements}.
```

Spawn `planner` subagent:
```
Draft the minimal ADR structure needed to explain the decision, alternatives, consequences, and tradeoffs.
```

Spawn `reviewer` subagent:
```
Review the proposed ADR for clarity, convention fit, decision focus, and unnecessary detail.
```

#### Advanced Research

For ADRs with structural or cross-module implications:

Spawn `architect` subagent:
```
Analyze module boundaries, ownership, and structural implications of the decision in {task}. Identify invariants and compatibility risks.
```

Spawn `auditor` subagent:
```
Audit the decision for production risks: data loss, migration hazards, rollback safety, and contract compatibility.
```

### Step 3 - Inspect Conventions

- Locate any existing ADR directory and naming conventions; inspect neighboring ADRs for title format, status values, headings, and detail level.
- Check project instructions in `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, or `CODEX.md`.

### Step 4 - Create ADR

- Use the project's ADR directory and naming conventions if present; otherwise, propose `doc/adr/` or `docs/adr/` before creating a new structure.
- Include Title, Status, Context, Decision, Consequences, and Alternatives where appropriate.
- Maintain a factual, decision-oriented record; avoid implementation logs or changelog-style text.
- Capture tradeoffs and rejected alternatives to clarify the rationale.

### Step 5 - Verify

- Run configured formatting, markdown linting, doc builds, or link checks if available.
- Otherwise, review generated docs for accuracy against the current code.

### Step 6 - Report

Report the following to the user:

1. **Decision Captured**
2. **Conventions Detected**
3. **ADR Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
