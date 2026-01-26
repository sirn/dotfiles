---
name: upgrade
description: Safely upgrade dependencies or migrate framework versions. Use when user asks to upgrade, update dependencies, or migrate to a new version.
---

Safely upgrade dependencies or migrate framework versions.

## Process
- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories

### Step 1 - Identify Upgrade Type
Ask the user to clarify what they want to upgrade:
- **Single dependency**: Upgrade one package (e.g., `react 18 -> 19`)
- **All dependencies**: Update all packages to latest compatible versions
- **Framework migration**: Major version upgrade with breaking changes (e.g., `Next.js 13 -> 15`)
- **Language version**: Update runtime version (e.g., `Python 3.11 -> 3.12`)

### Step 2 - Analyze Current State
1. Invoke `project-analyzer` skill to detect package manager and project type
2. Read the dependency file (package.json, pyproject.toml, Cargo.toml, go.mod, Gemfile, etc.)
3. Identify current versions of packages to be upgraded

### Step 3 - Research Changes
Spawn `best-practices-researcher` agent to research:
- Breaking changes in the target version
- Official migration guides
- Deprecated APIs that need updates
- Common pitfalls and solutions

Use WebSearch/WebFetch to find:
- Official changelogs
- Migration documentation
- Community experiences with the upgrade

### Step 4 - Generate Plan
Create an upgrade plan with:
- Deprecated API replacements needed
- Breaking changes to address
- Test updates required
- Migration commands to run

Present the plan to the user for approval before proceeding.

### Step 5 - Execute Upgrades
After user approval:
1. Update dependency file with new versions
2. Install new dependencies
3. Fix breaking changes in code
4. Invoke `test` skill to verify changes
5. Fix any test failures

### Step 6 - Fix Failures
For any test failures:
1. Identify root cause (breaking change or test issue)
2. Fix code or tests as appropriate
3. Re-run tests to verify

## Stop Condition
- If a fix fails twice, stop and ask for guidance.

## Output
1. **Upgrading** - What's being upgraded (package, version range, or all)
2. **Current Versions** - Before upgrade
3. **Target Versions** - After upgrade
4. **Breaking Changes** - From research (with migration steps)
5. **Migration Plan** - Step-by-step (presented before execution)
6. **Updates Applied** - Files changed, commands run
7. **Test Results** - From verification
8. **Failures Fixed** - If any (with explanations)
9. **Remaining Issues** - Requires manual intervention
