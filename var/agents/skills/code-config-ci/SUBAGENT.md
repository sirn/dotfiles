---
name: code-config-ci
description: Configure or debug CI/CD pipelines. Use when asked about GitHub Actions, GitLab CI, or build pipelines.
---

Configure, debug, and optimize CI/CD pipelines.

## Process
1. Identify context:
   - Identify the CI provider (GitHub Actions, GitLab CI, Jenkins, etc.)
   - Locate the config files (`.github/workflows/*.yml`, `.gitlab-ci.yml`, etc.)

2. Spawn agents:
   - `code-researcher`: "Lookup latest syntax/versions for {ci_provider} actions and tools"
   - `security-researcher`: "Check pipeline configuration for secret leaks, script injection, and insecure permissions"

3. Execute based on goal:
   - **New Pipeline**: Generate a standard pipeline (Lint -> Test -> Build -> Deploy)
   - **Debug**: Analyze logs (via WebFetch if URL provided) or error descriptions to fix steps
   - **Optimize**: Suggest caching strategies, parallelism, or container optimization

## Output
1. **Pipeline Configuration** (YAML/Groovy content)
2. **Security Checks** (Permissions, Secrets usage)
3. **Optimization Notes** (Cache keys, Docker image sizes)
