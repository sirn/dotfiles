---
name: code-upgrade
description: Safely upgrade dependencies or migrate framework versions. Use when user asks to upgrade, update dependencies, or migrate to a new version.
---

Safely upgrade dependencies or migrate framework versions.

## Process

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.

### Step 1 - Identify Upgrade Type

Ask the user to clarify what they want to upgrade if it is not already clear:

- **Single dependency**: Upgrade one package (e.g., `react 18 -> 19`).
- **All dependencies**: Update all packages to latest compatible versions.
- **Framework migration**: Major version upgrade with breaking changes (e.g., `Next.js 13 -> 15`).
- **Language version**: Update runtime version (e.g., `Python 3.11 -> 3.12`).

### Step 2 - Analyze Current State

1. Detect project type and package manager:
   - JavaScript/TypeScript: `package.json`, lockfiles, npm/yarn/pnpm/bun scripts.
   - Python: `pyproject.toml`, `requirements*.txt`, `uv.lock`, `poetry.lock`.
   - Rust: `Cargo.toml`, `Cargo.lock`.
   - Go: `go.mod`, `go.sum`.
   - Ruby: `Gemfile`, `Gemfile.lock`.
   - Nix: `flake.nix`, `flake.lock`, `shell.nix`; read the `nix` and `flake` skills.

2. Read dependency files and identify current versions.

3. Identify existing test, lint/check, build, and formatter commands from instructions, task runners, wrappers, and package manager scripts.

4. Note lockfiles but do not edit them manually.

### Step 3 - Research Changes

Spawn `researcher` subagent:

```
Research breaking changes for upgrading:
{package} from {current_version} to {target_version}

Find official migration guides, changelogs, and deprecated API replacements.

Identify common pitfalls, dependency manager commands, security advisories, and verification steps.
```

Then synthesize findings. Prefer official documentation and record sources.

### Step 4 - Generate Plan

Create an upgrade plan incorporating research findings. For broad, major-version, or risky upgrades, spawn the following agents before presenting the plan:

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

For framework migrations or upgrades spanning module boundaries, also spawn:

Spawn `architect` subagent:

```
Analyze module boundaries, ownership, dependency direction, and migration shape for upgrading {package}.
Recommend the minimal migration path that preserves invariants.
```

Include:

- Deprecated API replacements needed.
- Breaking changes to address.
- Dependency manager commands to run.
- Lockfile update strategy through the package manager only.
- Test/build/lint commands to run after migration.

Present the plan to the user for approval before proceeding when the upgrade is broad, major-version, or likely risky.

### Step 5 - Execute Upgrades

After approval when needed, delegate to `worker`:

```
Upgrade {package} from {current} to {target} in {project}.
Use {package_manager} commands.
Update dependency declarations, let the package manager update lockfiles.
Fix breaking changes identified in the migration plan: {summary}.
Run {test/lint commands} after each change.
```

### Step 6 - Fix Failures

For any failures the worker couldn't resolve:

Spawn `researcher` subagent:

```
Research this failure after upgrading {package}:
{error output}

Identify root cause and minimal fix.
```

Spawn `worker` subagent:

```
Apply this fix:
{findings}
```

Re-run the relevant command to verify.

### Step 7 - Report

Report the following to the user:

1. **Upgrading** — What's being upgraded
2. **Current Versions** — Before upgrade
3. **Target Versions** — After upgrade
4. **Breaking Changes** — From research with sources
5. **Migration Plan** — Step-by-step, presented before risky execution
6. **Updates Applied** — Files changed, commands run
7. **Verification Results** — Tests/build/lint/checks
8. **Failures Fixed** — If any, with explanations
9. **Remaining Issues** — Requires manual intervention

## Stop Condition

If a fix fails twice, stop and ask for guidance.
