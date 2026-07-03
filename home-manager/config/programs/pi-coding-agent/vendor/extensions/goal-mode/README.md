# Goal Mode Extension

Autonomous, goal-driven agent execution for the Pi Coding Agent.

## Overview

Goal mode turns a one-shot agent interaction into a persistent autonomous loop: the agent keeps working until the objective is complete, a budget is exhausted, or the user pauses/clears the goal. It is designed to mirror Codex's "keep going until the query is completely resolved" philosophy while adding optional budget limits and pause/resume controls.

### Design Principles (aligned with Codex)

1. **Tool-based completion is primary.** The agent calls the `complete_goal` model tool to mark the objective achieved (mirroring Codex's `update_goal`). This is more reliable than guessing from prose.
2. **Regex detection is a fallback.** If the agent declares completion in natural language without calling the tool, the harness still auto-completes via `detectCompletion()`.
3. **Trust the model for _when_ to stop.** Completion is detected by the agent's signal (tool call or declaration), not by harness-side heuristics about the codebase.
4. **Stall detection is a safety net.** If a continuation turn produces no tool calls AND no completion signal, the loop stops. This catches stuck agents without interfering with normal progress.
5. **Budgets are optional.** By default, goals have unlimited turns and cost. Budgets are a backstop, not a primary control.
6. **Context survives compaction.** After smart-compact runs, goal context is re-injected on the next `before_agent_start` so the agent doesn't lose the objective.

## Commands

| Command                   | Description                                    |
| ------------------------- | ---------------------------------------------- |
| `/goal <objective>`       | Set a new active goal and start driving it     |
| `/goal` or `/goal status` | Show current goal status and budget usage      |
| `/goal pause`             | Pause auto-continuation (goal state preserved) |
| `/goal resume`            | Resume a paused or budget-limited goal         |
| `/goal clear`             | Remove the current goal entirely               |
| `/goal complete`          | Mark the current goal as complete              |
| `/goal budget turns <N>`  | Set max turns (`unlimited` or `inf`)           |
| `/goal budget cost <N>`   | Set max cost in USD (`unlimited` or `inf`)     |

## Architecture

```
goal-mode/
├── index.ts              # Entry point (commands, hooks, and tools)
├── lib/
│   ├── contract.ts       # Types, state, completion detection (tool/regex)
│   ├── paths.ts          # Filesystem paths for prompts
│   └── cache.ts          # Stat-and-hash file cache
├── tests/
│   └── contract.test.ts  # Unit tests for contract.ts
└── README.md             # This file
```

### Key Flows

**Setting a goal** (`/goal <objective>`):

1. A `goal-state` custom entry is appended to the session branch.
2. A `goal-set` message is sent with `triggerTurn: true`, starting the first turn.
3. `before_agent_start` injects the active-goal context prompt (invisible).
4. The agent runs and `agent_end` fires.

**Resuming a goal** (`/goal resume`):

1. A fresh `goal-state` entry (status `active`) is appended, resetting the budget window.
2. A `goal-continuation` message is sent with `triggerTurn: true`, immediately restarting the agent loop (mirroring Codex's `ThreadResumed` auto-activation).
3. `before_agent_start` injects the active-goal context prompt.
4. The agent resumes work. Resuming from `complete` is rejected; resuming from `budget-limited` is allowed (the fresh budget window gives the agent more room).

**Auto-continuation** (`agent_end` hook):

1. If the turn was a self-triggered continuation, classify it via `classifyContinuation()` (three-tier, in priority order):
   - **`"complete"`** → agent called `complete_goal` OR declared completion in text → set status to `complete`, notify, stop. (The tool path normally completes inside the tool's `execute()` and returns early at the top of `agent_end`; this branch covers the regex fallback.)
   - **`"stalled"`** → no tool calls and no completion signal → notify, stop.
   - **`"continue"`** → the agent did real work → proceed to step 2.
2. Skip if compaction just happened or context is near threshold.
3. Check budget; if exhausted, send a budget-reached message and stop.
4. Otherwise, send a continuation message with `triggerTurn: true`.

**Compaction recovery** (`session_compact` hook):

When compaction fires during a continuation turn, `agent_end` skips sending another continuation (the `recentlyCompacted` guard). If the compacted turn won't be retried (`willRetry === false`, e.g. threshold or manual compaction), the goal loop would silently stall. The `session_compact` handler detects this case and re-triggers a continuation turn, mirroring Codex's `MaybeContinueIfIdle` which resumes after compaction. If `willRetry` is true, the retried turn's own `agent_end` handles continuation, so the handler skips.

**`pendingContinuationTurn` flag lifecycle:**

The `agent_end` hook sets `pendingContinuationTurn = true` when it sends a continuation. The `before_agent_start` hook must preserve this flag for continuation-triggered turns (so `agent_end` can detect them as self-triggered) but clear it for user-driven turns. It distinguishes the two by checking whether the leaf entry is a `goal-continuation` custom message.

**Budget tracking:**

- Budget is derived from all assistant messages after the LAST `goal-state` entry. Setting or resuming a goal appends a new entry, resetting the window.

## Completion Detection

The `classifyContinuation()` function in `contract.ts` consolidates the completion/stall check into a single testable decision. It uses three helpers, checked in priority order:

1. **`runCalledCompleteGoal()`** (PRIMARY) — checks whether any assistant message in the run made a `toolCall` to the `complete_goal` tool. This is the model-tool-based mechanism aligned with Codex's `update_goal`.
2. **`detectCompletion()`** (FALLBACK) — regex patterns on the last assistant message, kept as a safety net for models that miss the tool.
3. **`runHadToolCalls()`** — checks for any `toolCall` parts across all assistant messages in the run; absence indicates a stall.

The `complete_goal` tool is registered in `index.ts` via `pi.registerTool()` and conditionally activated via `pi.setActiveTools()` only while a goal is active (toggled in `updateGoalStatus`). This keeps the system prompt clean when no goal is running and prevents spurious completion calls.

When the agent calls it, the tool's `execute()` reads the current goal state, sets status to `complete`, updates the status bar, returns a success text (with final usage when a budget is set), and returns `terminate: true` to hint that pi should skip the follow-up LLM call — the goal is done and the agent should not continue. Because the tool completes the goal mid-turn, the `agent_end` hook's early `state.status !== "active"` check returns before reaching `classifyContinuation()` for the tool path; the regex fallback in `classifyContinuation()` only fires when the agent declared completion in text without calling the tool.

Completion patterns (fallback) match variations of:

- "The objective has been completed" / "is complete" / "is now complete"
- "The goal has been achieved" / "is done"
- "All requirements have been met" / "all tests are passing"
- "I have completed the objective"

False positives from sub-task language ("step 1 is done") are avoided by requiring the subject word (objective/goal/task) to be adjacent to the completion verb.

## Security: Objective Escaping and Validation

Objectives are user-provided text inserted into prompt XML tags (`<untrusted_objective>`). Three defenses aligned with Codex:

- **`escapeXmlText()`** — escapes `&`, `<`, `>` before insertion, preventing prompt-injection breakouts (e.g. an objective containing `</untrusted_objective>` cannot close the tag).
- **Function-replacement in `replaceAll`** — the escaped objective is passed as a **function** replacement (`() => escapeXmlText(...)`) to all prompt template substitutions, not a string replacement. String replacements interpret `$'`, `$&`, `` $` ``, and `$$` specially, which would let a `$'` in the objective expand to the template tail (containing a real closing tag) and break out of the `<untrusted_objective>` wrapper. Function replacements treat `$` literally, closing the vector.
- **`validateObjective()`** — rejects empty or over-length objectives (max 4000 Unicode code points, matching Codex's limit). Length is counted by code points, not UTF-16 units, so emoji-heavy objectives are measured correctly.

## Files

### Prompt Files

External prompt files live in `vendor/prompts/goal-mode/` and are deployed to `~/.pi/agent/custom/goal-mode/prompts/` by Nix. If the files are missing, inline fallbacks are used.

- **`goal-active.md`** (injected during `before_agent_start` when a goal is active)
  - Placeholders: `{OBJECTIVE}`, `{TURNS_USED}`, `{MAX_TURNS}`, `{COST_USED}`, `{MAX_COST}`
- **`goal-continue.md`** (injected during `agent_end` when sending a continuation)
  - Placeholders: `{OBJECTIVE}`, `{TURNS_REMAINING}`, `{COST_REMAINING}`
- **`goal-budget-reached.md`** (injected during `agent_end` when budget is exhausted)
  - Placeholders: `{OBJECTIVE}`

## Dependencies & API Reference

### Pi Core API Call Sites

- `pi.on("session_start", async (_event, ctx) => { ... })`
- `pi.on("turn_end", async (_event, ctx) => { ... })`
- `pi.on("session_shutdown", async (_event, ctx) => { ... })`
- `pi.on("before_agent_start", async (_event, ctx) => { ... })`
- `pi.on("agent_end", async (event, ctx) => { ... })`
- `pi.on("session_compact", async (event, ctx) => { ... })`

### Context & Harness APIs Used

- `pi.registerTool({ name: "complete_goal", ... })`
- `pi.setActiveTools(...)`
- `ctx.hasUI`
- `ctx.ui.setStatus("goal-status", ...)`
- `ctx.ui.notify(..., ...)`
- `ctx.ui.select(..., ...)`
