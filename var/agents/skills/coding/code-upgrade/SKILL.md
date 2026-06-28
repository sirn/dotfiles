---
name: code-upgrade
description: Safely upgrade dependencies or migrate framework versions. Use when user asks to upgrade, update dependencies, or migrate to a new version.
---

Safely upgrade dependencies or migrate framework versions.

## Process

- If code changes are involved, run `jj diff -s` to see changed files, then use `jj diff -- path` to restrict to specific files or directories.

### Step 1 - Identify Upgrade Type

Clarify the target if it is not already clear:

- **Single dependency**: One package (e.g., `react 18 -> 19`).
- **All dependencies**: All packages to their latest compatible versions.
- **Framework migration**: Major version upgrade with breaking changes (e.g., `Next.js 13 -> 15`).
- **Language version**: Runtime environment update (e.g., `Python 3.11 -> 3.12`).

### Step 2 - Analyze Current State

1. Detect project type and package manager:
   - JS/TS: `package.json`, lockfiles, and scripts.
   - Python: `pyproject.toml`, `requirements*.txt`, `uv.lock`, and `poetry.lock`.
   - Rust: `Cargo.toml` and `Cargo.lock`.
   - Go: `go.mod` and `go.sum`.
   - Ruby: `Gemfile` and `Gemfile.lock`.
   - Nix: `flake.nix`, `flake.lock`, and `shell.nix` (read the `nix` and `flake` skills).
2. Read dependency files to identify current versions.
3. Identify test, lint/check, build, and formatting commands from instructions, task runners, wrappers, and scripts.
4. Note lockfiles but do not edit them manually.

### Step 3 - Research Changes

Spawn the `researcher` subagent:

```
Research breaking changes for upgrading:
{package} from {current_version} to {target_version}

Find official migration guides, changelogs, and deprecated API replacements.

Identify common pitfalls, dependency manager commands, security advisories, and verification steps.
```

Synthesize findings, prioritizing official documentation and recording sources.

### Step 4 - Generate Plan

Develop an upgrade plan from research. For major, broad, or high-risk upgrades, spawn these agents first:

Spawn `reviewer` subagent:

```
Review the upgrade plan for {package} with a migration-risk lens:
- breaking changes
- behavior regressions
- project-convention risks
```

Spawn `auditor` subagent:

```
Audit the upgrade plan for {package} for production risks:
- data loss
- migration hazards
- rollback safety
- contract compatibility
```

For framework migrations or changes crossing module boundaries, also spawn:

Spawn `architect` subagent:

```
Analyze module boundaries, ownership, dependency direction, and migration shape for upgrading {package}.
Recommend the minimal migration path that preserves invariants.
```

The plan must address:

- Deprecated API replacements.
- Breaking changes to address.
- Dependency manager commands to run.
- Lockfile updates (via the package manager only).
- Post-migration verification commands (test, build, lint).

Seek user approval before executing broad or risky upgrades.

### Step 5 - Execute Upgrades

Once approved, delegate execution to the `worker` subagent:

```
Upgrade {package} from {current} to {target} in {project}.
Use {package_manager} commands.
Update dependency declarations, let the package manager update lockfiles.
Fix breaking changes identified in the migration plan: {summary}.
Run {test/lint commands} after each change.
```

### Step 6 - Fix Failures

If the worker cannot resolve a failure:

1. Spawn `researcher` to find a solution:

```
Research this failure after upgrading {package}:
{error output}

Identify root cause and minimal fix.
```

2. Spawn `worker` to apply the fix:

```
Apply this fix:
{findings}
```

3. Re-run verification commands to confirm.

### Step 7 - Report

Provide a final summary detailing:

1. **Upgrading** — Target dependencies.
2. **Current Versions** — Versions before the upgrade.
3. **Target Versions** — Upgraded versions.
4. **Breaking Changes** — Key changes identified with sources.
5. **Migration Plan** — Steps planned and executed.
6. **Updates Applied** — Files changed and commands executed.
7. **Verification Results** — Test, build, and lint outcomes.
8. **Failures Fixed** — Troubleshooting and resolution details.
9. **Remaining Issues** — Tasks requiring manual attention.

## Stop Condition

If a fix fails twice, stop and ask for guidance.
