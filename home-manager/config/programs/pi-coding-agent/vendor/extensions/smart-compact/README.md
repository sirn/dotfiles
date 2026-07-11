# Smart Compact Extension

High-fidelity session compaction and threshold-based auto-compaction for the Pi Coding Agent.

## Overview

Smart Compact replaces Pi's default, generic conversation history compaction behavior with a highly sophisticated, user-configured LLM summarization. It intercepts compaction requests (`session_before_compact`) to produce a dense, structured "Checkpoint Summary" using a designated high-performance model, ensuring crucial context is never lost. Additionally, it automates context management (`agent_settled`) by continuously monitoring token usage and triggers compaction when context size crosses user-defined limits.

### Design Principles

1. **Structured summarization preserves meaning.** Traditional compactions often omit crucial technical details. Smart Compact requests exact markdown checkpoints containing specific sections: Primary Objective, Technical Decisions & Findings, Execution History, Current Progress & State, Blockers & Open Questions, and Next Actions.
2. **Hysteresis-driven automation.** To prevent repeated compaction runs on subsequent turns, the auto-compaction trigger tracks state transitions. It only fires when token count crosses above the threshold from a clean or previously sub-threshold state.
3. **Robust, silent fallbacks.** If a configured compaction model is not available, API credentials are missing, or a completion error occurs, the extension automatically notifies the user and falls back to Pi's default built-in compaction, preventing any disruption to the agent's run.
4. **First-class integration with standard tools.** Smart Compact supports delegating summarization directly to specialized external tools like `pi-vcc` if enabled.

## Configuration

The extension is configured via a JSON file stored at `~/.pi/agent/custom/smart-compact/config.json`.

```json
{
  "provider": "anthropic",
  "model": "claude-3-5-sonnet-latest",
  "maxTokens": 4096,
  "autoCompact": {
    "enable": true,
    "maxContextTokens": 150000,
    "contextRatio": 0.8
  },
  "vcc": {
    "enable": false
  }
}
```

### Configuration Fields

- **`provider`**: The identifier of the LLM provider (e.g., `"anthropic"`, `"openai"`). Required unless `vcc.enable` is `true`.
- **`model`**: The model identifier used to generate the summary. Required unless `vcc.enable` is `true`.
- **`maxTokens`**: Optional limit on the length of the generated summary (defaults to `model.maxTokens`).
- **`autoCompact`**: Configuration for automated compaction triggers:
  - **`enable`**: Boolean flag to enable or disable auto-compaction.
  - **`maxContextTokens`**: An absolute token threshold (defaults to `150000`).
  - **`contextRatio`**: Ratio of the model's overall context window to use as a threshold (default `0.8`, must be in the range `(0, 1]`).
- **`vcc`**: Version control compaction settings. When `enable` is `true`, manual summary generation is bypassed and auto-compaction utilizes the `"__pi_vcc__"` instruction to delegate handling to `pi-vcc`.

The active compaction threshold is computed dynamically as the minimum of `autoCompact.maxContextTokens` and `floor(contextWindow * autoCompact.contextRatio)`.

## Architecture

```
smart-compact/
├── index.ts              # Entry point: registers event hooks
├── lib/
│   └── cache.ts          # Stat-and-hash based configuration file cache
└── README.md             # This file
```

### Key Flows

**Compaction Interception (`session_before_compact`):**

1. Pi initiates a compaction step (either manually or automatically).
2. The hook intercepts this event and loads the active configuration.
3. If `cfg.vcc?.enable === true` is configured, it returns early and allows `pi-vcc` to handle compaction.
4. The extension resolves the designated model via `ctx.modelRegistry.find` and retrieves authorization credentials via `ctx.modelRegistry.getApiKeyAndHeaders`.
5. If the model or key is missing, it displays a fallback notification and returns `undefined` to let Pi's default core compaction handle the turn.
6. The entire conversation history to be compacted is serialized into an LLM message format using `convertToLlm(...)` and `serializeConversation(...)`.
7. A prompt is constructed asking for a structured checkpoint summary containing exact file paths, commands, and status sections.
8. The extension calls `complete(model, { messages }, { apiKey, headers, signal })` to invoke the LLM. The provided `AbortSignal` is forwarded so compaction can be gracefully aborted.
9. On success, it returns `{ compaction: { summary, firstKeptEntryId, tokensBefore } }`.

**Auto-Compaction Trigger (`agent_settled`):**

The threshold check runs in `agent_settled`, the event Pi emits only after a run has fully settled — i.e. no automatic retry, auto-compaction retry, or queued continuation remains. Hooking here (rather than `agent_end`) avoids racing Pi's retry/continuation machinery: a compaction started in `agent_end` can be aborted mid-flight when Pi immediately continues with a queued follow-up message, leaving the context in a half-compacted state and the hysteresis tracker stuck above threshold.

1. After each settled agent run, the extension checks the current token usage using `ctx.getContextUsage()`.
2. It computes the active compaction threshold.
3. It compares the current token count against the threshold, referencing `previousTokens` to detect a crossing.
4. Before triggering compaction, it defensively requires `ctx.isIdle()` to be true. `agent_settled` already implies idle, but the check guards against another extension having started a new run during the preceding async config load.
5. If token usage crosses above the threshold, idle is confirmed, and an auto-compaction is not already in progress (`autoCompactionInProgress` guard), it triggers `ctx.compact({ customInstructions, onComplete, onError })`.
   - If VCC is enabled, `customInstructions` is set to `"__pi_vcc__"`.
   - Otherwise, a generic auto-compaction preservation prompt is used.
6. Once compaction completes, `previousTokens` is reset to `null` so the next above-threshold crossing can be detected on future runs. The same reset runs on error so a failed or aborted compaction can retry on the next settled run. The effective threshold is tracked across runs: when it changes (because the model's context window or the config ratio/cap changed), `previousTokens` is dropped so the hysteresis re-evaluates against the new threshold instead of staying permanently suppressed above the old one. The threshold tracker is reset alongside `previousTokens` in both callbacks.

## Files

- **`index.ts`**: The main entry point containing all event listeners, hysteresis token trackers, auto-compaction triggers, and LLM completion calls.
- **`lib/cache.ts`**: Implements a `memoizeByStat` function that caches the deserialized configuration file. It avoids redundant disk reads by tracking the file's size, modification time (mtime), and an FNV-1a hash of its content.
- **`~/.pi/agent/custom/smart-compact/config.json`**: The user-defined JSON configuration file.

## Dependencies & API Reference

### Pi Core API Call Sites

- `pi.on("agent_settled", async (_event, ctx) => { ... })`
- `pi.on("session_before_compact", async (event, ctx) => { ... })`

### Context APIs Used

- `ctx.getContextUsage()`
- `ctx.isIdle()` (defensive guard before triggering auto-compaction)
- `ctx.model` (to retrieve current model context limits)
- `ctx.modelRegistry.find(provider, modelId)`
- `ctx.modelRegistry.getApiKeyAndHeaders(model)`
- `ctx.compact({ customInstructions, onComplete, onError })`
- `ctx.ui.notify(...)` (safely executed only when `ctx.hasUI` is true)
- `ctx.hasUI`

### Pi Utilities Used

- `convertToLlm(allMessages)`
- `serializeConversation(...)` (imported from `@earendil-works/pi-coding-agent`)

### External Imports

- `complete(model, request, options)` (imported from `@earendil-works/pi-ai`)
- `node:os`, `node:path`

## Notable Implementation Details

- **Hysteresis and Token Tracking**: The tracking state `previousTokens` is crucial for preventing infinite compaction loops. Compaction is only triggered on the _transition_ from `<= threshold` to `> threshold`. Both the completion and error callbacks reset `previousTokens` to `null`, so a failed or aborted compaction can re-fire on the next settled run instead of permanently pinning the tracker above threshold. The effective threshold itself is tracked across runs: when it changes (model context window or config ratio/cap changed), `previousTokens` is reset so a lowered threshold does not leave the baseline permanently above it and suppress compaction forever. The threshold tracker is cleared in the same callbacks.
- **Retry/Continuation Safety**: The auto-compaction trigger runs in `agent_settled`, which Pi emits only after all automatic retries, auto-compaction retries, and queued continuations have drained. A defensive `ctx.isIdle()` check immediately before `ctx.compact(...)` guards against another extension starting a new run during the async config load. Together these ensure compaction never races a continuation turn that would abort it mid-flight.
- **VCC Integration**: When VCC is enabled (`vcc.enable === true`), manual checkpoint generation is bypassed during `session_before_compact`. During auto-compaction, the special marker instruction `"__pi_vcc__"` is sent to invoke the `pi-vcc` tool.
- **Prompt Design**: Because the checkpoint summary entirely replaces the compacted conversation history, the prompt explicitly instructs the LLM to output highly dense, technical details including raw commands, file paths, and diagnostic information rather than hand-waving abstractions.
