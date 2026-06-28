---
name: code-generate-ci
description: Generate or update CI/CD pipeline configuration. Use when asked to add or change GitHub Actions, GitLab CI, Jenkins, or other pipeline config.
---

Generate minimal CI/CD configurations matching existing project conventions.

## Process

### Step 1 - Identify Context

- If code changes are involved, run `jj diff -s` first to view changed files, then use `jj diff -- path` to restrict to specific paths.
- Focus on any specified provider, platform, branch policy, deployment target, or commands.
- Identify the requested pipeline goal (e.g., lint, check, test, build, release, or deploy).

### Step 2 - Research and Plan

To research, plan, and review, spawn the appropriate subagents:

- **researcher**:

```
Research current official CI provider syntax, action versions, permission requirements, and cache patterns for {provider}.
```

- **planner**:

```
Design a minimal CI pipeline for the requested goal using the project's existing commands and conventions.
```

- **reviewer**:

```
Review the proposed CI design for secret leaks, script injection, unsafe permissions, insecure pull-request handling, and unnecessary complexity.
```

### Step 3 - Inspect Conventions

- Locate existing CI/CD config files, project task runners, and any instruction files (`README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`).
- Ensure syntax, action versions, permissions, and cache patterns align with current official provider guidelines.

### Step 4 - Generate CI Config

- Generate a minimal pipeline for the requested goal (typically lint/check → test → build → deploy only when required).
- Prefer existing wrapper scripts and documented task-runner commands.
- Use least-privilege permissions, avoid exposing secrets, and mitigate risks like script injection, unsafe pull-request handling, and poor cache keys.

### Step 5 - Verify

- Run YAML formatting, schema validation, provider-specific checks, or dry-run tools when available.
- If local validation is unavailable, explain manual verification steps and any platform-specific risks.

### Step 6 - Report

Report the following details:

1. **Pipeline Goal**
2. **Conventions Detected**
3. **CI/CD Config Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
