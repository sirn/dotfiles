---
name: code-review-api
description: Verify API or library usage against authoritative documentation using specialized subagents when available. Use when asked to check API correctness, integration correctness, or version-specific library behavior.
---

Verify API and library usage against authoritative documentation using specialized agents.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified APIs, libraries, versions, files, or paths, focus on those.
   - Identify APIs, versions, packages, generated clients, SDKs, or protocols in use.

2. Spawn applicable agents in parallel:
   - `researcher`: "Verify API/library usage in {files or context} against official documentation, including version-specific parameters, behavior, configuration, and deprecations."
   - `reviewer`: "Review the API/library usage findings for correctness risk and minimal remediation."

3. Read relevant code yourself to validate and synthesize agent findings:
   - Check local instructions and conventions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md` when present.
   - Read relevant implementation, dependency manifests, generated types, lockfiles when needed, and tests.
   - Prefer existing project patterns unless documentation proves they are incorrect.

4. Verify against authoritative documentation:
   - Research official documentation with WebSearch/WebFetch or project-approved documentation tools when needed.
   - Check version-specific changes, parameter names, types, expected behavior, configuration, error handling, and deprecations.
   - Compare usage against documentation and identify correctness risks.

5. Synthesize findings:
   - Prioritize issues that can cause incorrect behavior, incompatibility, data loss, or unsupported usage.
   - Include file paths and line references or quoted snippets.
   - Provide concrete fixes and verification steps.

## Output

1. **Executive Summary**
2. **APIs / Libraries Checked**
3. **Documentation Sources**
4. **Findings** prioritized Critical > High > Medium > Low
5. **Evidence** with file paths and line references
6. **Recommended Remediation**
7. **Verification Steps**
