Create a detailed implementation/execution plan based on the user instruction.
Write the plan to: {PLAN_PATH}

## Pre-Planning Phase (REQUIRED — complete ALL steps in order before writing the plan)

### Step 1: Evaluate Existing Plan

Check whether a plan file already exists at: {PLAN_PATH}

- If it exists: read it and evaluate whether it is relevant to the current user request.
  - If relevant: acknowledge it and build on or refine it.
  - If not relevant: discard it (overwrite with a fresh plan).
- If it does not exist: proceed to step 2.

### Step 2: Gather Context

Collect all context necessary to successfully accomplish the request:

- Read the project README and any relevant configuration or source files.
- **Perform a web search**: load the relevant search skill and execute it. Do not guess or assume.
- **Research official documentation** for every library, tool, or API involved. Look up exact call conventions, flags, and return types — do not infer from memory.
- If any requirement is ambiguous or information is missing, **ask the user** before proceeding.

### Step 3: Define Success Criteria

Derive explicit, measurable success criteria from the user request:

- What does "done" look like? (e.g., "tests pass", "command exits 0", "output matches expected format")
- If the criteria are non-obvious or involve tradeoffs, **confirm them with the user** before writing the plan.

### Step 4: Write the Plan

Only after completing steps 1–3, write the plan to: {PLAN_PATH}

### Step 5: Verify the Plan

Before finalising, validate the plan's correctness:

- **Re-consult official documentation** to confirm every call convention, option, and API contract used in the plan.
- **Run ad-hoc read-only probes** where helpful (e.g., `ls`, `cat`, `--help`, dry-runs, `tsc --noEmit`) to verify assumptions without modifying the system.

## Rules

- CRITICAL: Use ONLY read-only commands for context gathering and verification. Do NOT execute changes.
- Use the `write` tool to write the plan file. Do NOT use bash to write files (they will be blocked).
- **Always cite documentation**: include a URL or reference for every external tool, API, or convention referenced in the plan.
- Do NOT write any code yet. Just create the plan file.
- You MUST use only the provided plan file path. Any attempt to write elsewhere will be blocked.

## User instruction

{USER_PROMPT}

## Plan structure

### Overview

What needs to be built/fixed, why, and how success is measured. What is OUT of scope. Keep the solution minimal.

### Success Criteria

Explicit, measurable criteria that define when the implementation is complete.

### Context

Read-only exploration findings: key files, existing patterns, dependencies, non-obvious details, and relevant URLs.
Example: "`src/auth.ts` exports `createSession(userId)`; sessions stored in Redis with 24h TTL"

### Implementation Steps

Ordered, atomic, verifiable steps. Each step needs: a clear goal, specific files/changes, and a concrete success criterion (e.g., test command, linter run).
Describe non-trivial changes in sufficient detail to prevent guesswork. Show before/after with ±5 lines of context if helpful.
Example: "Step 1: Add Session type to types.ts — Success: tsc --noEmit passes"

### Verification Checklist

How to verify the complete implementation after all steps are done.

### References

Documentation URLs and sources consulted during planning.

## Policy footer

- Once the plan is written, present a concise summarisation of the plan to the user.
