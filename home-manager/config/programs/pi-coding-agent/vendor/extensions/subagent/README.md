# Subagent Extension

Delegate task orchestration to specialized child agents with isolated contexts, sequential step-dependency routing, and parallel execution.

## Overview

The Subagent extension exposes a single Pi tool named `subagent` that enables delegation of complex or multi-faceted tasks to specialized child agents. It coordinates their execution across a two-dimensional step matrix, handles input/output routing, and provides real-time progress, cost, and session tracking directly inside the Pi TUI.

### Core Capabilities

1. **Flexible Orchestration:** Tasks are provided as a two-dimensional `steps` array. Inner tasks in a step run in parallel, while outer steps run sequentially.
2. **Context Routing:** The accumulated output from one step can flow dynamically into the next step via `<previous>` tag interpolation or appending.
3. **Pluggable Runners:** Supports both the native Pi RPC runner (`pi --mode rpc`) and the Claude Code runner (`claude --print --output-format stream-json`).
4. **Rich TUI Progress:** Displays live status, intermediate thought streams, tool usage, accumulated costs, changed files, and resumed session IDs in the Pi terminal interface.

---

## Architecture

```
subagent/
├── index.ts              # Entry: registers tool, hooks, step logic
├── types.ts              # Interfaces: AgentConfig, SingleResult, Runner
├── utils.ts              # Utilities: ANSI stripping, temp files, sessions
├── pi-runner.ts          # Pi RPC runner (JSONL stream, proxy UI)
├── claude-code-runner.ts # Claude Code runner (NDJSON stdout parsing)
└── lib/
    ├── cache.ts          # FNV-1a stat-signature directory listing cache
    └── perf.ts           # PI_EXTENSION_PERF gated timing measurements
```

---

## Tool and API Surface

The extension registers a single Pi tool and hooks into the active session lifecycle. It does not expose any custom slash commands or message renderers beyond the tool's custom `renderCall` and `renderResult` implementations.

### Tool Registration

```ts
pi.registerTool({
  name: "subagent",
  label: "Subagent",
  description:
    "Delegate tasks to specialized subagents with isolated context. " +
    "Supports both Pi and Claude Code runners.",
  parameters: SubagentParams,
  async execute(_toolCallId, params, signal, onUpdate, ctx) { ... },
  renderCall(args, theme, _context) { ... },
  renderResult(result, { expanded, isPartial }, theme, context) { ... },
});
```

- **Parameters:** Accepts a 2-D array of tasks `steps: TaskItem[][]`. Each `TaskItem` defines:
  - `agent` (string): The identifier of the agent to invoke.
  - `task` (string): The prompt instructing the agent what to do.
  - `cwd` (string, optional): Directory path to run the task in.
  - `sessionId` (string, optional): An existing session ID to resume.

### Hook Registrations

The extension hooks into the session start, shutdown, and turn-activation events:

```ts
pi.on("session_start", (_event, ctx) => {
  if (ctx.hasUI) restoreSubagentCost(ctx);
});
pi.on("session_shutdown", (_event, ctx) => {
  if (ctx.hasUI) ctx.ui.setStatus("subagent-cost", undefined);
});
pi.on("before_agent_start", (_event, ctx) => { ... });
```

### Status Bar and Context Integration

- **Status Bar Item:** Tracks session costs under the `"subagent-cost"` key using `ctx.ui.setStatus`. It is restored on startup and cleared on shutdown.
- **Context Restoration:** If a subagent execution is aborted by the parent, any completed step results are captured via `pi.appendEntry("subagent-partial-results", { status: "pending", summary })`. On the next turn, `before_agent_start` injects this as a custom context entry (`subagent-partial-results-context`) back into the prompt so the parent retains visibility of the subagent's progress.

---

## Runtime Flow

```
[Tool Execution]
       │
       ▼
 [Agent Discovery]  ◄── Cache validation (memoizeDirectoryByStat)
       │
       ▼
 [Config Loading]   ◄── ~/.pi/agent/custom/subagent/config.json
       │
       ▼
[Step Orchestration]
  For each step in steps:
   ├── Normalize Session IDs & Validate constraints
   ├── Inject Previous Step Output (<previous> tags)
   ├── Run Parallel Batch (mapWithAgentConcurrency)
   │     │
   │     ├──► [Pi Runner] ────► Spawn "pi --mode rpc"
   │     └──► [Claude Runner] ─► Spawn "claude -p ..."
   │
   ├── Check Failures (Skip future steps on failure)
   └── Aggregate usage & Extract changed files
       │
       ▼
[Return AgentToolResult]
```

### 1. Agent Discovery

Upon each invocation of `execute()`, the extension locates active agent definitions:

- **Lookup Path:** Discovered in `agents/` relative to `getAgentDir()` (typically `~/.pi/agent/agents/*.md`).
- **Frontmatter Configuration:** Parsed from each Markdown file's YAML frontmatter:
  - `name` (required string): Agent ID.
  - `description` (required string): Agent purpose.
  - `tools` (comma-separated): Allowed tools list.
  - `model` (string): Model ID overrides.
  - `concurrency` (number): Limit of concurrent instances for this agent.
  - `mode` (string): Execution-mode overrides.
  - `thinkingLevel` (string): Effort/thinking setting. Passed as `model:thinkingLevel` for Pi, or `--effort` for Claude Code.
  - `runner` (string): Runner selection (`"pi"` or `"claude-code"`, defaulting to `"pi"`).
  - The remaining Markdown body serves as the agent's `systemPrompt`.
- **Caching:** Discovery uses FNV-1a hashes combined with filesystem stats via `memoizeDirectoryByStat` (`lib/cache.ts`). Modifying an `.md` file instantly invalidates the directory cache on the next run.

### 2. Config Loading

Loads additional settings from `~/.pi/agent/custom/subagent/config.json`:

- `maxConcurrency` (default 4, ceiling 16): Maximum global concurrent agents.
- `maxAgentsPerStep` (default 8, ceiling 32): Maximum agents running in a single parallel step.
- `collapsedItemCount` (default 3): Max visible tasks before rendering as collapsed in the TUI.
- `agentConcurrency`: A key-value map overriding individual agent limits. Setting an agent's limit to `0` clears its frontmatter-specified limit.

### 3. Step Orchestration

Orchestration sequences steps sequentially and runs tasks within a step in parallel:

- **Plan Initialization:** Pre-populates all planned tasks in the UI state as `SingleResult` items (initially marked as waiting/pending) so the entire execution layout is immediately visible to the user.
- **Session ID Normalization:** Ignores string-coerced session IDs (like `"null"`, `"undefined"`, `"none"`, or `"nil"`). Rejects duplicates of the same session ID within a single step to prevent runner collision.
- **Input Injection:** Before executing a step, the accumulated outputs of the previous step are routed to the next step. If `<previous>` exists in the task description, it is replaced with the preceding output. If not, the preceding output is appended to the prompt inside `<previous>...</previous>` tags.
- **Execution:** Parallel tasks are chunked in batches of size `maxAgentsPerStep` and executed via `mapWithAgentConcurrency`. This controls both global concurrency (`maxConcurrency`) and individual agent-level limits.
- **Live Updates:** Progress updates are emitted to the UI via `onUpdate` as the runners stream tokens.
- **Failure Handling:** If an agent fails (exits non-zero or crashes), all subsequent steps in the orchestrator are marked as `skipped` in the TUI. Accumulated costs are saved, and the tool returns an error result.

### 4. Runner Dispatch

For each task in the active step, the runner selection matches the agent's `runner` field and dispatches execution:

```ts
const RUNNERS: Record<AgentConfig["runner"], AgentRunner> = {
  pi: runPiAgent,
  "claude-code": runClaudeCodeAgent,
};
```

#### Execution Mode Inheritance

Child processes inherit parent execution modes:

1. Derives mode from the `PI_EXECUTION_MODE` environment variable or the latest custom `execution-mode` entry in the active session.
2. Appends `"subagent"` and `"subagent:${agent.name}"` to ensure children can be identified downstream.
3. Appends the agent's frontmatter `mode` if overridden.

---

## Runner Implementations

### Pi RPC Runner (`pi-runner.ts`)

The Pi runner executes tasks by spawning a child Pi instance running in RPC mode.

- **Command Spawned:**
  ```bash
  pi --mode rpc
  ```
- **Arguments:** Passes `--session <id>`, `--session-dir <path>` (isolated sub-directory calculated from the parent session filename), `--model`, `--tools`, and `--append-system-prompt` (pointing to a temp file containing the agent's system prompt).
- **Communication:** Standard I/O using newline-delimited JSON RPC events.
- **Key Events:**
  - `message_start`, `message_delta`, `message_end`: Accumulates and yields the streaming assistant messages.
  - `tool_execution_start`, `tool_execution_end`: Captures active tool usage.
  - `turn_start`, `turn_end`: Controls step execution. Turn start and work events cancel an active 2-second grace timer.
  - `agent_end`: Begins a 2-second grace period. If no further work events (such as compactions or retries) fire before the timer expires, the process safely finalizes.
  - `auto_retry_start`/`end`, `compaction_start`/`end`: Reflects state changes and cancels any active grace periods.
  - `extension_ui_request`: Proxies interactive subagent prompts (select, confirm, notify) straight up to the parent Pi terminal interface using `ctx.ui.select(...)` or `ctx.ui.notify(...)`.
- **Abortion:** If aborted by the parent signal, the runner sends an RPC `abort` message, then issues `SIGTERM`, and finally `SIGKILL` after 5 seconds if the process hasn't exited.

### Claude Code Runner (`claude-code-runner.ts`)

The Claude Code runner spawns Claude CLI headless sessions.

- **Command Spawned:**
  ```bash
  claude -p "Task: ${task}" --output-format stream-json --verbose
  ```
- **Arguments:** Passes `--resume <sessionId>`, `--model`, `--effort` (from thinkingLevel), `--tools`, `--append-system-prompt-file` (via temp file), and `--permission-mode bypassPermissions` (ensures fully headless run with no terminal prompts).
- **Communication:** Reads streamed NDJSON from stdout. Stdin is not written to.
- **Key Events:**
  - `system`: Initializes the session ID and model.
  - `assistant`: Mapped directly to Pi `Message` instances via `buildAssistantMessage`. Thinking blocks are skipped, and token usage is accumulated.
  - `user`: Represented as tool result messages.
  - `result`: Definitive finish event containing final cost, usage, and stop reasons. Overrides intermediate usage metrics and triggers termination after a 5-second grace period.
- **Abortion:** Issues standard `SIGTERM`, following up with `SIGKILL` if necessary (does not support an RPC abort).

---

## Notable Implementation Details

### Result Lifecycle

All task items start in an initial pending state initialized via `createPendingResult` with `exitCode: -1`. The orchestrator and renderer utilize type guard helpers (`isPendingResult`, `isSkippedResult`, `isFailedResult`) to manage control flow and dynamically change progress icons.

### Concurrency and Semaphores

`mapWithAgentConcurrency` limits executing tasks by combining a global concurrency semaphore with individual agent semaphores. If an agent limit is reached, other tasks for that agent wait, while tasks for other agents continue running in parallel up to the global `maxConcurrency` cap.

### Output Integration and Formatting

- **XML Extraction:** The combined output of a step is extracted, cleaned of terminal ANSI escape codes, and processed into structured blocks.
- **File Change Extraction:** `extractFileChanges` scans child agent tool calls for file-mutating operations (`write`, `Write`, `edit`, `Edit`, `MultiEdit`). It logs unique changed file paths and aggregates them into the final `<output-meta>` metadata block of the parent tool result.
- **Output Truncation:** Large result bodies are truncated using `truncateHead` to respect `DEFAULT_MAX_BYTES` and `DEFAULT_MAX_LINES`. If truncated, a temporary file containing the full, untruncated content is written, and its path is exposed to the parent.

### TUI Rendering & Performance Optimizations

- **Render Elements:** Custom TUI component `SubagentResultView` (a `Box` subclass) renders step lists.
  - Nerd Font glyphs represent execution states (waiting, pending, running, skipped, failed, success, retrying, compacting).
  - Task previews display a collapsed view by default with an expand hint `keyHint("app.tools.expand", "to expand")`.
  - Renders cumulative token usage, execution time, and individual step costs.
- **Memoization:** To keep streaming updates fast and prevent frame drops during intensive multi-agent parallel streaming, the TUI uses several layers of memoization:
  - A `WeakMap` cache groups results together.
  - Display lists are cached per result keyed by message length.
  - Result object structures are memoized using a fast content-version string key.
