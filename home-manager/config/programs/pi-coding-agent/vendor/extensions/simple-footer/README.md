# Simple Footer Extension

A compact, information-dense custom footer for Pi's text user interface.

## Overview

The Simple Footer extension replaces Pi's built-in terminal footer with a highly optimized, two-line layout. It provides a real-time, glanceable dashboard of your session's state—including the current working directory, cumulative token usage, cache statistics, session cost, context window utilization, compaction warnings, active model name, thinking level, and status indicators exposed by other active extensions.

### Core Features

1. **Two-Line Layout:** Maximizes vertical terminal space compared to bulkier default footers.
2. **Detailed Session Statistics:** Computes and formats cumulative input, output, cache read, and cache write tokens, alongside total session cost.
3. **Cross-Extension Integration:** Automatically reads and displays status indicators from the `plan-mode`, `shell-policy`, `subagent`, and `goal-mode` extensions.
4. **Compaction Awareness:** Warns you with a highlight when the branch was recently compacted, preserving context-window context.
5. **Caching & Efficiency:** Caches generated lines to bypass recomputation when terminal dimensions and session variables remain unchanged.

## Architecture

```
simple-footer/
├── index.ts    # Entry point: footer renderer & branch listeners
└── README.md   # This file
```

### Key Flows

**Session Startup and Installation** (`session_start` hook):

1. The extension listens for `session_start`.
2. It checks `ctx.hasUI`. If the agent is running in non-interactive or print modes, the extension immediately exits to avoid overhead.
3. If UI is active, it defines an internal state accumulator (`usage`) and recomputes cumulative token and cost metrics by traversing the branch.
4. It registers a custom footer component via `ctx.ui.setFooter(...)`.
5. It captures a reference to the active `TUI` instance (`activeTui`).
6. It subscribes to branch changes via `footerData.onBranchChange(...)` to trigger recalculation whenever new messages arrive.

**Render Loop** (`render(width)` callback):

1. The footer renderer is called with the current terminal `width`.
2. It retrieves dynamic inputs: the formatted CWD, context usage, model ID, thinking level, and cross-extension statuses.
3. It constructs a unique signature from these inputs. If the signature and width match the `renderCache`, it immediately returns the cached lines.
4. Otherwise, it compiles the two footer lines:
   - **Line 1 (CWD & Statuses):**
     - Left: Dim-colored formatted path (e.g., replacing home folder with `~`).
     - Right: Spaced status indicators (`execution-mode` and `goal-status`).
   - **Line 2 (Metrics & Model Info):**
     - Left: Space-separated cumulative token counts (input, output, cache-read, cache-write), cost, subagent cost, context usage ratio, and a compaction warning.
     - Right: Active model ID and thinking level.
5. It pads both lines to the terminal width using visible (non-escape) length.
6. It truncates both lines to fit within the `width` while preserving ANSI codes.
7. It saves the lines to `renderCache` and returns them.

**Lifecycle Refresh and Cleanup**:

- On `turn_end` and `agent_end` hooks, the extension recomputes usage and requests a fresh render via `activeTui.requestRender()`.
- On `session_shutdown` hook, if `ctx.hasUI` is active, it calls `ctx.ui.setFooter(undefined)` to gracefully restore Pi's built-in footer.

## User-Facing Surface

The Simple Footer is visible only when running Pi in interactive TUI mode. It displays two highly structured lines at the bottom of the screen:

**Line 1 Layout:** `[Formatted CWD]                     [Execution Mode] [Goal Status]`

**Line 2 Layout:**

`↑[In] ↓[Out] R[CacheR] W[CacheW] $[Cost] ([SubCost])` `ctx:[Used]/[Total] (compact)  [Model] • [Level]`

### Metric Displays

- **Token Formatting:** Numbers are rendered compactly: under 1k as-is, under 10k with one decimal (e.g., `8.5k`), under 1M as rounded thousands (e.g., `45k`), and 1M+ with a decimal (e.g., `1.2M`).
- **Cumulative Cost:** Formatted to two decimal places (e.g., `$0.15`).
- **Subagent Cost:** Shown in parentheses next to main costs if the subagent extension is active.
- **Context Utilization:** Displays actual used tokens against total context window size (e.g., `ctx:2.4k/128k`).
- **Compaction Warning:** Renders a yellow `(compact)` tag if a smart-compaction event occurred within the last three branch entries.
- **Thinking Level:** Appends `• <level>` next to the active model ID if the active model supports a thinking parameter and it is not `"off"`.

## Files

`index.ts` : The entire extension code. Contains hook bindings, usage accumulation, formatting helpers, and the custom TUI footer rendering logic.

## Dependencies and API Integration

The extension leverages the following types and functions:

### Imports

- `ExtensionAPI`, `getLatestCompactionEntry` from `@earendil-works/pi-coding-agent`
- `AssistantMessage` from `@earendil-works/pi-ai`
- `TUI` from `@earendil-works/pi-tui`
- Node `path` and `process.env.HOME`

### Pi API Call Sites

- `pi.on("session_start", (event, ctx) => { ... })` - Sets up footer on start.
- `pi.on("turn_end", () => { ... })` - Triggers recalculation and redraw.
- `pi.on("agent_end", () => { ... })` - Triggers recalculation and redraw.
- `pi.on("session_shutdown", (event, ctx) => { ... })` - Removes footer.
- `pi.getThinkingLevel()` - Retrieves current thinking level config.
- `ctx.ui.setFooter((tui, theme, footerData) => { ... })` - Installs custom footer renderer.
- `ctx.ui.setFooter(undefined)` - Clears footer on exit.

### Context and TUI Helper API Usage

- `ctx.hasUI` - Verifies interactive terminal mode.
- `ctx.cwd` - Obtains current working directory.
- `ctx.model?.id` - Identifies the active model.
- `ctx.getContextUsage()` - Reads context token metrics.
- `ctx.sessionManager.getBranch()` - Traverses current branch for usage metrics.
- `footerData.onBranchChange(callback)` - Subscribes to session history updates.
- `footerData.getExtensionStatuses()` - Obtains cross-extension status maps.
- `tui.requestRender()` - Explicitly notifies the terminal to redraw the screen.

## Cross-Extension Statuses

The `simple-footer` is a consumer of status values. It does not write statuses itself. It reads the following keys from `footerData.getExtensionStatuses()`:

`execution-mode` : Set by `plan-mode`, `shell-policy`. Rendered via `statuses.get("execution-mode")` (accent colored).

`subagent-cost` : Set by `subagent`. Rendered via `statuses.get("subagent-cost")` (parenthesized cost).

`goal-status` : Set by `goal-mode`. Rendered via `statuses.get("goal-status")` (dim/muted status text).

## Notable Implementation Details

- **Branch Metric Aggregation:** The extension manually tallies the session cost and token counts by traversing the full active branch retrieved via `ctx.sessionManager.getBranch()`. It filters only for entries of type `"message"` where `role === "assistant"` and aggregates their `usage` fields.
- **Recent Compaction Detection:** Compaction awareness uses `getLatestCompactionEntry(branch)` and flags the session as recently compacted only if the compaction event occurred within the last three entries of the history branch. When active, it displays the warning string in the theme's `warning` color (typically yellow or orange).
- **Theming and Colors:** Leverages the TUI `theme` mapping to style sections:
  - `theme.fg("dim", ...)` for default metrics, CWD, and model information.
  - `theme.fg("accent", ...)` for `execution-mode`.
  - `theme.fg("muted", ...)` for `goal-status`.
  - `theme.fg("warning", ...)` for `(compact)`.
- **ANSI-Aware Width Manipulation:** Because normal string operations (like `.length` and `.slice`) count hidden ANSI escape sequences, the extension implements custom formatting helpers:
  - `visibleWidth(s)` strips ANSI sequences `\x1b\[[0-9;]*m` to accurately measure printed width.
  - `truncateToWidth(...)` truncates visually while keeping ANSI formatting tags intact, ensuring terminal output never wraps or corrupts terminal colors.
- **Performance Caching:** Generates a cache key signature comprising fourteen dynamic state parameters (CWD, token counts, costs, compaction, active model, and statuses). If the terminal width and the signature have not changed, it serves the pre-rendered lines directly, preventing redundant string format and padding calculations.
