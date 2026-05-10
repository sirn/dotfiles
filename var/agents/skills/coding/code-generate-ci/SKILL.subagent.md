---
name: code-generate-ci
description: Generate or update CI/CD pipeline configuration using specialized subagents when available. Use when asked to add or change GitHub Actions, GitLab CI, Jenkins, or other pipeline config.
---

Generate minimal CI/CD configuration that matches existing project conventions.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified a provider, platform, branch policy, deployment target, or commands, focus on those.
   - Identify the requested pipeline goal, such as lint, check, test, build, release, or deploy.

2. Spawn applicable agents in parallel:
   - `researcher`: "Research current official CI provider syntax, action versions, permission requirements, and cache patterns for {provider}."
   - `planner`: "Design a minimal CI pipeline for the requested goal using the project's existing commands and conventions."
   - `reviewer`: "Review the proposed CI design for secret leaks, script injection, unsafe permissions, insecure pull-request handling, and unnecessary complexity."

3. Inspect existing conventions yourself:
   - Locate existing CI/CD config files and project task runners.
   - Check project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
   - Research current official provider syntax, action versions, permissions, and cache patterns when needed.

4. Generate CI/CD configuration:
   - Generate a minimal pipeline for the requested goal, typically lint/check → test → build → deploy only when required.
   - Prefer existing wrapper scripts and documented task-runner commands.
   - Use least-privilege permissions and avoid exposing secrets.
   - Check for script injection, unsafe pull-request handling, and poor cache keys.

5. Apply changes only within the requested scope.

6. Verify:
   - Run YAML formatting, schema validation, provider-specific validation, or dry-run tools when available.
   - If local validation is unavailable, explain the manual verification steps and any remaining platform-specific risks.

## Output

1. **Pipeline Goal**
2. **Conventions Detected**
3. **CI/CD Config Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
