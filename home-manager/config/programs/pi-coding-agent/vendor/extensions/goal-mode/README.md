# Goal Mode Extension

Autonomous, goal-driven agent execution for the Pi Coding Agent.

## Overview

Goal mode turns a one-shot agent interaction into a persistent autonomous loop: the agent keeps working until the objective is complete, a budget is exhausted, or the user pauses/clears the goal. It follows a "keep going until the query is completely resolved" philosophy while adding optional budget limits and pause/resume controls.

### Design Principles

1. **Tool-based completion is primary.** The agent calls the `update_goal` model tool to mark the objective achieved or blocked.
2. **No regex fallback, no stall detection.** A continuation turn with no tool calls is trusted to continue. Continuation is gated solely by `status === "active"`.
3. **Trust the model for _when_ to stop.** Completion is detected by the agent's `update_goal` tool call, not by harness-side heuristics.
4. **Turn errors map to blocked/usage-limited.** Provider/transport failures (stopReason "error" or "aborted") are mapped to "blocked" (or "usage-limited" for rate/billing errors). This prevents the loop from re-firing on a failing turn.
5. **In-place objective updates.** Running `/goal <new-objective>` while a goal is active/paused/blocked/etc. updates the objective in place without confirmation. Confirmation only appears when the existing goal is complete or cleared (truly starting fresh).
6. **Budgets are optional.** By default, goals have unlimited turns and cost. Budgets are a backstop, not a primary control.
7. **Context survives compaction.** After smart-compact runs, goal context is re-injected on the next `before_agent_start`.

## Commands

| Command | Description |
| --- | --- |
| `/goal <objective>` | Set a new active goal or update an active goal's objective in place |
| `/goal` or `/goal status` | Show current goal status and budget usage |
| `/goal pause` | Pause auto-continuation (goal state preserved) |
| `/goal resume` | Resume a paused, budget-limited, blocked, or usage-limited goal |
| `/goal clear` | Remove the current goal entirely |
| `/goal complete` | Mark the current goal as complete |
| `/goal budget turns <N>` | Set max turns (`unlimited` or `inf`) |
| `/goal budget cost <N>` | Set max cost in USD (`unlimited` or `inf`) |

_Note: If a goal is already active, paused, blocked, usage-limited, or budget-limited, running `/goal <objective>` updates the objective in-place without asking for confirmation, immediately pivoting the agent if active._

## Statuses

| Status | Set by | Resumable | Terminal | Description |
| --- | --- | --- | --- | --- |
| `active` | `/goal`, `/goal resume`, tool (`update_goal`) | n/a | no | Goal is actively running and driving autonomous turns. |
| `paused` | `/goal pause` | yes | no | Paused by the user; state and budget are preserved. |
| `blocked` | tool (`update_goal`), turn-error | yes | no | Blocked on a repeating impasse (3 strikes) or a generic turn error. |
| `usage-limited` | turn-error (rate/billing limit) | yes | no | Hit a provider rate limit, billing quota, or token capacity. |
| `budget-limited` | system (budget exhausted) | yes | no | Turn or cost budget was fully consumed. |
| `complete` | tool (`update_goal`), `/goal complete` | no (start fresh) | yes | Goal successfully accomplished. |
| `cleared` | `/goal clear` | no (start fresh) | yes | Goal removed from tracking. |

## Architecture

```
goal-mode/
├── index.ts              # Entry point (commands, hooks, and tools)
├── lib/
│   ├── contract.ts       # Types, state, turn-error detection, completion detection (tool-based only)
│   ├── paths.ts          # Filesystem paths for prompts
│   └── cache.ts          # Stat-and-hash file cache
├── tests/
│   └── contract.test.ts  # Unit tests for contract.ts
└── README.md             # This file
```

### Key Flows

**Setting a goal** (`/goal <objective>`):

- If no goal exists or the existing goal is complete/cleared: confirms replacement, appends a fresh `goal-state` entry (status `active`, default budget), sends a `goal-set` message with `triggerTurn: true`.
- If the existing goal is active/paused/blocked/usage-limited/budget-limited: updates the objective in place (appends a `goal-state` entry with the new objective, keeping the existing status and budget) without confirmation. For active goals, sends a `goal-objective-updated` message with `triggerTurn: true` to pivot the agent. For non-active goals, notifies the user to `/goal resume`.

**Resuming a goal** (`/goal resume`):

1. A fresh `goal-state` entry (status `active`) is appended, resetting the budget window.
2. A `goal-continuation` message is sent with `triggerTurn: true`, immediately restarting the agent loop (resumed-thread auto-activation).
3. `before_agent_start` injects the active-goal context prompt.
4. The agent resumes work. Resuming from `complete` is rejected; resuming from `budget-limited` is allowed (the fresh budget window gives the agent more room).

**Auto-continuation** (`agent_end` hook):

1. **Turn-error detection**: Inspect the last assistant message for `stopReason === "error" | "aborted"`. If found, set status to `"blocked"` (or `"usage-limited"` for rate/billing errors) and stop.
2. **Completion detection** (continuation turns only): If the agent called `update_goal` with `status=complete`, set status to `"complete"` and stop.
3. Skip if compaction just happened.
4. Check budget; if exhausted, send a budget-reached message and stop.
5. Otherwise, send a continuation message with `triggerTurn: true`.

**Compaction recovery** (`session_compact` hook):

When compaction fires during a continuation turn, `agent_end` skips sending another continuation (the `recentlyCompacted` guard). If the compacted turn won't be retried (`willRetry === false`, e.g. threshold or manual compaction), the goal loop would silently stall. The `session_compact` handler detects this case and re-triggers a continuation turn, which resumes after compaction. If `willRetry` is true, the retried turn's own `agent_end` handles continuation, so the handler skips.

**`pendingContinuationTurn` flag lifecycle:**

The `agent_end` hook sets `pendingContinuationTurn = true` when it sends a continuation. The `before_agent_start` hook must preserve this flag for continuation-triggered turns (so `agent_end` can detect them as self-triggered) but clear it for user-driven turns. It distinguishes the two by checking whether the leaf entry is a `goal-continuation` custom message.

**Budget tracking:**

- Budget is derived from all assistant messages after the LAST `goal-state` entry. Setting or resuming a goal appends a new entry, resetting the window.

## Completion Detection

Completion is detected solely via the `update_goal` model tool: when the agent calls `update_goal` with `status=complete`, the tool's `execute()` sets the goal status to `"complete"`, updates the status bar, and returns `terminate: true` to hint that pi should skip the follow-up LLM call.

The `classifyContinuation()` function in `contract.ts` serves as a safety net for the case where the tool was called but the turn continued (mixed tool batch where not all results set `terminate: true`). It checks `runCalledCompleteGoal()` — whether any assistant message contains a `toolCall` to `update_goal` with `input.status === "complete"` — and returns `"complete"` if so, otherwise `"continue"`.

There is no regex-based completion fallback and no stall detection. Continuation is gated solely by `status === "active"`.

The `update_goal` tool is registered in `index.ts` via `pi.registerTool()` and conditionally activated via `pi.setActiveTools()` only while a goal is `active` or `budget-limited` (toggled in `updateGoalStatus`). This keeps the system prompt clean when no goal is running and prevents spurious completion calls, while still allowing a budget-limited goal to be marked complete (the sticky-rule allows `budget-limited` to be completed but not blocked).

When the agent calls it, the tool's `execute()` reads the current goal state, sets status to `complete` (or `blocked`), updates the status bar, returns a success text (with final usage when a budget is set), and returns `terminate: true` to hint that pi should skip the follow-up LLM call — the goal is done and the agent should not continue. Because the tool completes the goal mid-turn, the `agent_end` hook's early `state.status !== "active"` check returns before reaching `classifyContinuation()` for the tool path.

The tool renders its result via `renderCall` and `renderResult` inside pi's default tool shell (no `renderShell: "self"`), matching how the subagent extension composes its tool box. `renderCall` returns an empty `Text` (zero lines), suppressing the redundant "update_goal status=..." title row — the result body header alone is self-explanatory. `renderResult` returns a transparent `Box(0,0)` (no own background; the shell provides `toolSuccessBg`/`toolErrorBg` framing) containing a colored bold header (" goal complete" / " goal blocked" / " goal update rejected") followed by the usage summary (final turn/cost when a budget is set, otherwise a short status line). Collapsed shows only the usage with an expand hint; expanded additionally reveals the original objective. Error results have no objective and show the error text in both states (no expand hint), using the `error` color for the header so rejected updates are distinguishable. The spacing matches `makeGoalBoxRenderer` exactly (header → 1 blank → usage → 1 blank → objective/hint).

## Security: Objective Escaping and Validation

Objectives are user-provided text inserted into prompt XML tags (`<untrusted_objective>`). Three defenses:

- **`escapeXmlText()`** — escapes `&`, `<`, `>` before insertion, preventing prompt-injection breakouts (e.g. an objective containing `</untrusted_objective>` cannot close the tag).
- **Function-replacement in `replaceAll`** — the escaped objective is passed as a **function** replacement (`() => escapeXmlText(...)`) to all prompt template substitutions, not a string replacement. String replacements interpret `$'`, `$&`, `` $` ``, and `$$` specially, which would let a `$'` in the objective expand to the template tail (containing a real closing tag) and break out of the `<untrusted_objective>` wrapper. Function replacements treat `$` literally, closing the vector.
- **`validateObjective()`** — rejects empty or over-length objectives (max 4000 Unicode code points). Length is counted by code points, not UTF-16 units, so emoji-heavy objectives are measured correctly.

## Files

### Prompt Files

External prompt files live in `vendor/prompts/goal-mode/` and are deployed to `~/.pi/agent/custom/goal-mode/prompts/` by Nix. If the files are missing, inline fallbacks are used.

- **`goal-active.md`** (injected during `before_agent_start` when a goal is active)
  - Placeholders: `{OBJECTIVE}`, `{TURNS_USED}`, `{MAX_TURNS}`, `{COST_USED}`, `{MAX_COST}`
- **`goal-continue.md`** (injected during `agent_end` when sending a continuation)
  - Placeholders: `{OBJECTIVE}`, `{TURNS_REMAINING}`, `{COST_REMAINING}`
- **`goal-budget-reached.md`** (injected during `agent_end` when budget is exhausted)
  - Placeholders: `{OBJECTIVE}`
- **`goal-objective-updated.md`** (sent when the user updates the objective of an active goal)
  - Placeholders: `{OBJECTIVE}`, `{TURNS_REMAINING}`, `{COST_REMAINING}`

## Dependencies & API Reference

### Pi Core API Call Sites

- `pi.on("session_start", async (_event, ctx) => { ... })`
- `pi.on("turn_end", async (_event, ctx) => { ... })`
- `pi.on("session_shutdown", async (_event, ctx) => { ... })`
- `pi.on("before_agent_start", async (_event, ctx) => { ... })`
- `pi.on("agent_end", async (event, ctx) => { ... })`
- `pi.on("session_compact", async (event, ctx) => { ... })`

### Context & Harness APIs Used

- `pi.registerTool({ name: "update_goal", ... })`
- `pi.setActiveTools(...)`
- `ctx.hasUI`
- `ctx.ui.setStatus("goal-status", ...)`
- `ctx.ui.notify(..., ...)`
- `ctx.ui.select(..., ...)`
