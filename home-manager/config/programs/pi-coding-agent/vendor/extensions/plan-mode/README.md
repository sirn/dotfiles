# Plan Mode Extension

A structured, iterative planning environment for the Pi Coding Agent.

## Overview

Plan mode introduces a dedicated preparation and drafting phase before any code modifications begin. When activated, the agent is instructed to focus entirely on analyzing the task and documenting a concrete execution plan in a markdown file on disk, rather than jumping directly into editing. The user can then review, manually refine, and ultimately accept the plan. Once approved, the plan is injected into the agent's context to guide precise execution.

### Design Principles

1. **Structured drafting is superior to ad-hoc reasoning.** Keeping plans isolated in a physical document (`~/.pi/agent/plans/...`) allows both the user and the agent to iteratively refine the roadmap without polluting the main conversation history with discarded drafts.
2. **Flexible acceptance flavors.** Upon accepting a plan, users can choose whether to execute it immediately, run a compaction step to clean the working context before starting, or spin up a clean session to prevent any prior clutter from affecting execution.
3. **Context and mode isolation.** The agent's focus is explicitly governed by state. Dynamic prompt injection ensures the agent knows exactly when it is in "plan" mode (to write and structure files) versus when it has exited to "edit" mode (to execute the agreed plan).
4. **Resiliency and cross-session persistence.** Planning state and pending executions are stored in custom session branch entries. This allows plans to survive session recreation or agent restarts.

## Commands

`/plan` : Enter plan mode. Optional trailing text is forwarded to the agent as a prompt to begin generating the plan.

`/plan accept [message]` : Accept the drafted plan. Opens a modal prompt to choose how to execute the plan (immediate, compact, or fresh session).

`/plan show` : Open the active plan in the interactive editor for manual review or direct modification.

`/plan cancel` : Exit plan mode and return to normal editing. Prompts the user to optionally delete the draft plan file.

## Architecture

```
plan-mode/
├── index.ts          # Extension entry point: commands, hooks,
│                      # and message renderers
├── lib/
│   ├── contract.ts       # Modes, custom entries, and state persistence helpers
│   ├── paths.ts          # Filesystem paths for plans and prompts
│   └── cache.ts          # Stat-and-hash based file memoization cache
└── README.md             # This file
```

### Key Flows

**Entering Plan Mode** (`/plan`):

1. The user runs `/plan [args]`.
2. The extension switches the state to `MODE_PLAN` by writing an `EXECUTION_MODE_ENTRY` (`"execution-mode"`) custom entry to the session.
3. The plan file is created under the project-specific path if it does not already exist.
4. If arguments were provided, they are sent as a user message to guide the initial plan draft. If not, an informative notification is shown.
5. The UI status bar is updated to show `"plan mode"` with an icon (`uf4a0`).

**Context Injection & Transition Hooks** (`before_agent_start`):

1. Before the agent starts a turn, the hook checks the current execution mode.
2. If in `MODE_PLAN` and the most recent instruction was not already plan context, it loads the `plan-mode.md` prompt and appends an invisible custom message of type `PLAN_MODE_CONTEXT` (`"plan-mode-context"`) to guide the agent's behavior.
3. If not in plan mode but the last active instruction was a plan context, it loads `plan-mode-exit.md` and appends an invisible custom message of type `PLAN_MODE_EXIT` (`"plan-mode-exit"`) to signal the shift back to normal editing.

**Accepting a Plan** (`/plan accept`):

1. The extension verifies that the plan file exists and is at least 50 bytes to prevent executing empty or trivial plans.
2. The user is prompted with a modal selection (`ctx.ui.select`) to choose the acceptance flavor:
   - **Accept plan (and compact)**: Triggers session compaction via `ctx.compact` with the auto-compaction instruction, then immediately injects the plan. Note: If the session has already been recently compacted (detected via `isRecentlyCompacted(ctx)`), this option is replaced by "Accept plan" as the primary choice.
   - **Accept plan (no compaction)**: Directly injects the plan into a follow-up execution turn.
   - **Accept plan in a clean session**: Calls `ctx.newSession` to spin up a fresh conversation. To bridge the transition, a custom entry of type `"plan-execution-pending"` with `status: "pending"` is written, and the plan file is moved to the new session's folder.
3. On execution, a custom message of type `PLAN_MODE_EXECUTE` (`"plan-mode-execute"`) is generated containing the plan's contents.

**Cross-Session Resuming** (`session_start` hook):

1. When a new session starts, the extension scans the branch for any `"plan-execution-pending"` entry with `status: "pending"`.
2. If found, it restores the previous active model via `pi.setModel(model)`.
3. It appends a processed marker (`"processed"` status) to the branch.
4. It reads the plan file and automatically calls `sendExecutionMessage(...)` to inject the plan and trigger the agent's turn.

### UI and Status Indication

- **Status Bar**: Imperatively updated with `ctx.ui.setStatus("execution-mode", modeLabel(getMode(ctx)))`. While in plan mode, this displays `"plan mode"` alongside a custom icon (`uf4a0`).
- **Interactive Editor**: Reviewing or editing plans via `/plan show` opens the physical markdown file using `ctx.ui.editor("Plan", content)`. Changes are automatically saved back to disk.
- **Custom Message Renderer**: The `PLAN_MODE_EXECUTE` custom message is rendered with a stylized box:
  - When collapsed, it shows a brief summary instruction and a key hint.
  - When expanded, it displays the full markdown content of the plan.
  - Stylings are drawn using `@earendil-works/pi-tui` container widgets and colored dynamically using the active terminal theme.

## Files

### Prompt Files

Prompt files live in `~/.pi/agent/custom/plan-mode/prompts/`. If any prompt is missing on disk, hardcoded fallbacks are used.

`plan-mode.md` : Injected on `before_agent_start` (entering plan mode). Restricts the agent from making edits and guides it to draft a physical plan.

`plan-mode-exit.md` : Injected on `before_agent_start` (exiting plan mode). Notifies the agent that plan mode has ended and it can now write code.

`plan-mode-execute.md` : Injected on `PLAN_MODE_EXECUTE` (approving the plan). Injects the plan file content and instructs the agent to begin execution.

## Dependencies & API Reference

### Pi Core API Call Sites

- `pi.registerCommand("plan", { description, getArgumentCompletions, handler })`
- `pi.registerMessageRenderer("plan-mode-execute", (message, { expanded }, theme) => { ... })`
- `pi.on("session_start", async (_event, ctx) => { ... })`
- `pi.on("turn_end", async (_event, ctx) => { ... })`
- `pi.on("before_agent_start", async (_event, ctx) => { ... })`
- `pi.sendMessage({ customType: "plan-mode-execute", ... }, { triggerTurn: true })`
- `pi.sendUserMessage(args)`
- `pi.appendEntry("plan-execution-pending", { status: "processed" })`
- `pi.setModel(model)`
- Inside state management: `pi.appendEntry("execution-mode", { mode, policyOverride })`

### Context APIs Used

- `ctx.ui.setStatus`, `ctx.ui.notify`, `ctx.ui.select`, `ctx.ui.editor`
- `ctx.sessionManager.getBranch`, `ctx.sessionManager.getEntries`, `ctx.sessionManager.getSessionFile`, `ctx.sessionManager.appendCustomEntry`
- `ctx.newSession({ parentSession, setup })`
- `ctx.compact({ customInstructions, onComplete, onError })`
- `ctx.cwd`, `ctx.model`, `ctx.modelRegistry.find(provider, modelId)`

### External Imports

- `@earendil-works/pi-coding-agent` (`keyHint`, `getLatestCompactionEntry`, `ExtensionAPI`, `ExtensionContext`, `ExtensionCommandContext`)
- `@earendil-works/pi-tui` (`Container`, `Text`, `Box`, `Spacer`)
- `node:fs/promises`, `node:path`

## Notable Implementation Details

- **Plan Directory Structure**: Plan paths are deterministically mapped by taking the user's `cwd`, stripping leading slashes, and replacing all other slashes with dashes to create a flat subfolder structure: `~/.pi/agent/plans/--<normalized-project-dir>--/<session-id>.md`.
- **Cross-Device Robustness**: When moving plan files across sessions, the extension first attempts `node:fs/promises` `rename()`. If this fails (e.g. due to crossing partition or mount boundaries), it safely falls back to manually copying the content and deleting the source file.
- **State Scan Caching**: Branch scans (for determining the latest compaction or active execution mode) are memoized using a cache key structured as `${branch.length}:${leafEntryId}`. This ensures instant lookup without redundantly traversing the branch history on every hook execution.
- **Config & Prompt Loading**: Prompts are loaded using a `memoizeByStat` helper. This cache avoids redundant disk IO by checking the file's modification time (mtime), file size, and an FNV-1a hash of its content.
