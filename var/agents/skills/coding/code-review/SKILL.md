---
name: code-review
description: Review code for correctness, quality, conventions, simplicity, security, and overall health. Use when asked for a general or full code review.
---

Run a full code review or quality check using specialized agents.

## Process

### Step 1 - Identify Context

- For code changes, run `jj diff -s` to list changed files, and use `jj diff -- path` to inspect specific paths.
- Focus on user-specified files or paths when provided.
- Identify the spec source: look for issue references in commit messages (e.g. `#123`, `Closes #45`), PRD/spec files under `docs/`, `specs/`, or `.scratch/`, a user-supplied path, or ask the user. If no spec is identified, skip the spec review axis and note it in the final report.
- Identify standards sources: locate anything in the repo documenting how code should be written (e.g., `CONTRIBUTING.md`, `CODING_STANDARDS.md`, `docs/CONTRIBUTING.md`, or linter configurations that encode conventions) and pass these to the `reviewer` subagent.

### Step 2 - Spawn Review Agents

Spawn the `reviewer` subagent:

```
Review {files} across these lenses:
- correctness/quality — bugs, logic errors, edge cases, error handling, resource leaks, concurrency, and performance traps
- security — OWASP risks, injection flaws, auth/authz, cryptography, sensitive data exposure, dependency risks, and secure defaults
- convention/simplicity — assess using the 24 Fowler code smells; read the `code-smells` reference and apply each relevant smell's fix (smells are heuristics, never hard violations).

  Binding Rules:
  - The repo overrides: a documented repo standard (from identified standards sources {standards_sources}) always wins; where it endorses something the `code-smells` reference would flag, suppress the smell.
  - Always a judgement call: each smell is a labelled heuristic, never a hard violation. Skip anything tooling already enforces.

Cite the specific source (file and rule) for any violation of identified repo standards {standards_sources}.
```

Spawn the `researcher` subagent for the Spec review (if a spec is identified):

```
Compare {files} against the identified spec source {spec_source} and check:
- Requirements that are missing or only partially implemented.
- Behavior in the diff that was not requested in the spec (scope creep).
- Requirements that look implemented but where the implementation is incorrect.

Quote the relevant line from the spec for each finding.
```

Spawn the `researcher` subagent for API verification:

```
Verify relevant API/library usage in {files} against official documentation and research best practices.
```

### Step 3 - Audit

For production-bound changes, spawn the `auditor` subagent:

```
Audit {files} for production readiness: correctness, security, data loss, migration hazards, and rollback safety.
```

### Step 4 - Synthesize Findings

Read relevant code yourself to validate and synthesize agent findings.
- Keep the Spec review and the Code Quality review on strictly separate axes. Never merge or rerank them; a perfectly written implementation of the wrong feature must remain visible as a spec failure.
- When synthesizing code-quality findings, distinguish hard violations of documented standards from judgement calls on baseline code smells.

### Step 5 - Run Verification

Run verification commands only when the user requested full checks:

- Detect test, lint, check, and format commands from project instructions, task runners (`Makefile`, `justfile`, `Taskfile.yml`), wrapper scripts (`bin/`, `.my/bin/`), package manager scripts, or common defaults.
- Run appropriate non-destructive commands with timeouts.
- Report failures; modify files only if explicitly requested.

### Step 6 - Report

Produce the review report with the following structure:

#### Axis A: Specification Compliance
*(Skip or note if no spec source was found. Keep strictly separate from code quality.)*
1. **Requirements Gap**: Quoted spec lines for requirements that are missing or partially implemented.
2. **Scope Creep**: Behaviors in the diff that were not requested in the spec.
3. **Incorrect Logic**: Quoted spec lines where the code attempts to implement a requirement but does so incorrectly.

#### Axis B: Code Quality Review
1. **Executive Summary**
2. **Critical Issues** (must fix)
3. **Security Analysis**
4. **API / Documentation Verification**
5. **Quality & Logic**
6. **Simplicity & Convention**
   - **Hard Violations**: Documented repo standard breaches (cite specific file and rule).
   - **Judgement Calls**: the 24 Fowler code smells from the `code-smells` reference (treat as heuristics/suggestions, never hard violations; skip if tooling already enforces them).
7. **Performance & Best Practices**
8. **Test/Lint Results** (only when run)
9. **Quick Wins**
10. **Action Items** prioritized Critical > High > Medium > Low

Prioritize real, evidenced issues over speculative concerns. Include file paths, line references, or quoted snippets, and provide concrete fixes with verification steps.
