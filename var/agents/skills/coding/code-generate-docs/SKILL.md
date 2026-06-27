---
name: code-generate-docs
description: Generate or update documentation. Use when asked to add comments, docstrings, module docs, README sections, or reference documentation.
---

Generate documentation that matches existing project conventions.

## Process

### Step 1 - Identify Context

- Check for code changes by running `jj diff -s` first, then target specific files/directories with `jj diff -- path`.
- Focus on any user-specified files, modules, target audience, or requirements.
- Identify the documentation type (e.g., inline comments, docstrings, module/file docs, README sections, or reference guides).
### Step 2 - Research and Plan

Spawn `scout` subagent:

```text
Identify existing documentation style, naming, structure, examples, and relevant code context for {files or requirements}.
```

Spawn `planner` subagent:

```text
Draft the minimal documentation structure needed to explain the behavior, intent, data flow, or tradeoffs.
```

Spawn `reviewer` subagent:

```text
Review the proposed documentation for clarity, convention fit, accuracy, and unnecessary detail.
```

### Step 3 - Inspect Conventions

- Inspect adjacent code and documentation to match layout, heading style, naming conventions, and depth of detail.
- Check project instructions in `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, or `CODEX.md`.
### Step 4 - Generate Documentation

- Match existing documentation style and structure.
- Explain design intent and *why* choices were made, rather than describing obvious mechanics.
- Provide concise, verified examples based on actual code.
- Avoid changelog-style comments.

### Step 5 - Verify

- Run formatting, markdown linting, doc builds, or link checks if configured and relevant.
- Otherwise, review the new documentation against current code for accuracy.
### Step 6 - Report

Report the following to the user:

1. **Documentation Scope**
2. **Conventions Detected**
3. **Documentation Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
