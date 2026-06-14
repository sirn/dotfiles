---
name: code-generate-docs
description: Generate or update documentation. Use when asked to add comments, docstrings, module docs, README sections, or reference documentation.
---

Generate documentation that matches existing project conventions.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files, modules, audience, or requirements, focus on those.
- Determine the documentation type: inline comments, docstrings, module/file docs, README content, or reference material.

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

- Read relevant code and neighboring documentation.
- Check existing documentation for naming, layout, heading style, examples, and detail level.
- Check project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

### Step 4 - Generate Documentation

- Match existing documentation style and structure.
- Write comments that explain why and intent, not obvious mechanics.
- Prefer concise examples that can be verified from the current code.
- Avoid changelog-style comments.

### Step 5 - Verify

- Run configured formatting, markdown lint, docs build, or link checks when available and relevant.
- Otherwise, review generated docs for accuracy against the current code.

### Step 6 - Report

Report the following to the user:

1. **Documentation Scope**
2. **Conventions Detected**
3. **Documentation Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
