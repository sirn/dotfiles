# Shell Policy Extension

A comprehensive command-authorization gate and execution-control extension for the Pi Coding Agent.

## Overview

The `shell-policy` extension is a command-authorization gate designed to intercept tool calls, evaluate them against a structured policy, and ensure secure, intended agent behavior. Primarily, it intercepts `bash` tool calls, parses the execution command string, and maps it against a JSON-based policy to decide whether the tool may run (`allow`), must ask the user (`ask`), or must be blocked (`deny`).

In addition to command-level checking, the extension controls write and edit tools via execution-mode policies, manages project-local policy overrides, integrates an LLM-based "auto mode" to silently approve safe commands, and provides a "YOLO mode" that automatically executes non-destructive commands. Every decision is audit-logged to preserve historical execution details.

### Core Principles

1. **Safety First (Fail-Closed).** If a command cannot be parsed, contains unsupported syntax, or doesn't match any policy rules, it fails closed by falling back to a user-confirmation prompt (`ask`) or direct blocking.
2. **Context-Aware Enforcement.** Policies can vary depending on the active execution mode (e.g., `MODE_EDIT` or `MODE_YOLO`). This ensures stricter rules apply while allowing flexible, safe command execution.
3. **No Redundant Prompts.** Safe commands are automatically approved, and if "auto mode" is enabled, an LLM checks if the command matches safety guidelines before bothering the user with a confirmation dialog.
4. **Transparent Audit Logging.** All decisions, whether allowed, denied, or YOLO-approved, are logged locally with detailed context.

---

## User-Facing Surface & Configuration

### Commands

| Command           | Description                                     |
| ----------------- | ----------------------------------------------- |
| `/yolo [on\|off]` | Toggle YOLO mode on or off (toggles if no arg). |

### Status Bar and UI Interaction

- **Status Bar Icon:** Displays the active mode via the status item named `execution-mode` utilizing the mode's visual label (e.g., YOLO mode icon).
- **Notifications:** Displays warnings and policy updates using `ctx.ui.notify(...)`.
- **Confirmation Prompts:** When a command evaluates to `ask` or `default` and is not pre-approved, the extension blocks execution and asks the user: `ctx.ui.select("Confirm...\n<command>", ["Yes, proceed", "No, cancel"])`.

### Configuration and Policy Files

- **Global Policy Configuration** (`policy.json`): Located at `~/.pi/agent/custom/shell-policy/policy.json`. Defines the unified policy configuration mapping execution modes to active rules.
- **Project-Local Overrides** (`.pi/policy.json`): Located at `<cwd>/.pi/policy.json`. Defines project-specific command and wrapper rules merged on top of the active global policy stack.
- **Auto Mode Configuration** (`config.json`): Located at `~/.pi/agent/custom/shell-policy/config.json`. Configures the target LLM provider, model, timeouts, and tokens for silent safety pre-approval.
- **Auto Mode Templates**:
  - `auto-mode/prompt.md`: General LLM system prompt containing the placeholders: `{CONTEXT_HINT}`, `{COMMAND}`, `{CWD}`, and `{COMMANDS_CONTEXT}`.
  - `auto-mode/commands.md`: Command context reference.
  - `auto-mode/<mode>.md`: Context templates loaded per active execution mode (e.g., `yolo.md`).
- **YOLO Prompt Context** (`prompts/yolo-mode.md`): Located at `~/.pi/agent/custom/shell-policy/prompts/yolo-mode.md`. Custom system prompt injected during agent starts in YOLO mode. Falls back to an internal hard-coded prompt if missing.
- **Audit Logs** (`commands.log`): Located at `~/.pi/agent/logs/shell-policy/commands.log`. Records all decisions with timestamps and rules. Written with secure file permissions (`0700` directory / `0600` log file).

### Unified Policy Schema

```ts
interface UnifiedPolicyConfig {
  default: ModePolicy;
  modes?: Record<string, ModePolicy>;
}

interface ModePolicy {
  tools?: Record<string, boolean>; // e.g. { edit: false, write: false }
  commands: PolicyCommands;
  wrappers?: WrapperRuleConfig[];
  redirects?: RedirectPolicy;
  heredocs?: HeredocPolicy;
}

interface PolicyCommands {
  allow: CommandEntry[];
  ask: CommandEntry[];
  deny: CommandEntry[];
}

interface CommandEntry {
  match: string;
  mode: "exact" | "prefix" | "substring" | "args";
}

interface WrapperRuleConfig {
  name: string;
  kind: "shell-c" | "utility-operand" | "env" | "xargs" | "docker-run";
}

interface RedirectPolicy {
  action: Action; // "allow" | "ask" | "deny"
  safeTargets?: string[];
  allowFdDup?: boolean;
}

interface HeredocPolicy {
  action: Action;
}
```

#### Rule Matching Modes

- `exact`: The entire trimmed command string matches `match` (ignoring case).
- `prefix`: The command starts with `match` followed immediately by a word boundary.
- `substring`: The exact contiguous sequence of tokens in `match` exists anywhere in the parsed command.
- `args`: Formatted as `programPrefix:arg1 arg2...`. The program prefix matches the start of the command (or `*` matches any command), and all arguments listed must appear as tokens somewhere after the matched prefix.

---

## Architecture & Core Flows

```
shell-policy/
├── index.ts                     # Extension entry point: thin re-export
├── shell-policy.ts              # Core logic: registrations, hooks, flows
├── lib/
│   ├── shell-policy.ts          # Policy engine: tokenizer, extraction
│   ├── execution-mode.ts        # Mode stack, constants, state parsing
│   ├── path-hint.ts             # Compares write/edit target paths
│   └── paths.ts                 # Directory and filesystem path definitions
├── scripts/
│   └── evaluate-shell-policy.ts # Standalone interactive CLI policy debugger
└── tests/
    ├── path-hint.test.ts        # Unit tests verifying 18 path hint cases
    └── shell-policy.test.ts     # ~2600-line suite: tokenizer, rules, overrides
```

### Runtime Evaluation Flow

When the agent triggers a tool call, the `tool_call` hook intercepts it and executes the following multi-stage validation flow:

```
[Tool Call Intercepted]
         │
         ├──► Is tool "write" or "edit"?
         │     ├── Yes: Is it disabled in modePolicies?
         │     │     ├── Yes: Is the target path allowed by overrides?
         │     │     │     ├── Yes ──► [Allow Tool Call]
         │     │     │     └── No  ──► [Block Tool Call] (shows path-hint)
         │     │     └── No ──────────► [Allow Tool Call]
         │     └── No ──► Continue
         │
         └──► Is tool "bash"?
               ├── No: Continue (Other tools evaluated against tools list)
               └── Yes:
                    ├── 1. Load project-local .pi/policy.json (cached per cwd)
                    ├── 2. Merge stack: default -> modePolicies -> projectPolicy
                    ├── 3. Parse command string into command segments
                    ├── 4. Evaluate nested & unwrapped commands
                    └── 5. Handle evaluation result (allow, deny, ask, default)
```

#### 1. Pre-Execution Phase & Tool-List Filtering

The active execution mode is determined by parsing the `PI_EXECUTION_MODE` environment variable (comma-separated list) or evaluating previous session entries. At session start (`session_start`) and before the agent runs (`before_agent_start`), the `updateActiveTools` helper computes disabled tools from the current mode's policy:

1. It reads the current available tools via `pi.getActiveTools()`.
2. It snapshots this list into `savedPreFilterTools`.
3. It filters out disabled tools and sets the rest via `pi.setActiveTools(...)`.
4. After execution finishes (`agent_end` or `session_shutdown`), it restores the original state by merging the snapshot and active tools to preserve any newly registered tools.

#### 2. Mode Overrides and Path Violations

If a tool like `write` or `edit` is called while disabled in the active execution mode, the extension checks if the targeted path is allowed by `policyOverride.write` or `policyOverride.edit`. If not, execution is blocked. The helper `buildPathDiffHint` compares the target path against allowed paths using `path.resolve()` and returns a detailed segment diff, such as: `section 3, "baz" -> "hoge"` or `section 6, "--project-c--" -> either "--project-a--" or "--project-b--"`.

#### 3. Command Evaluation

For `bash` tool calls, the command string is tokenized and evaluated:

- The global `default` policy, the active `mode` policies, and any cached project-local policy from `<cwd>/.pi/policy.json` are merged using `mergeEvaluationPolicyStackStrict()`.
- The command is unwrapped to find any underlying command chains.
- Nested and direct commands are checked against rules.

#### 4. Decisions and Action Priority

When evaluating a command segment or chain, actions are resolved in order of priority:

- **Action Precedence:** `deny` > `ask` > `allow` > `default`.
- **Phase Precedence:** `commands` > `redirects` > `heredocs`.
- **Chains:** For command chains (e.g., `cd build && make`), if any nested segment has no matching rule, the decision is downgraded to `default` (requiring user confirmation or LLM approval) unless a stricter `ask` or `deny` was already triggered.

#### 5. Interactive Confirmation & Auto Mode

If a decision evaluates to `ask` or `default`, the confirmation flow triggers:

- **YOLO Mode Bypass:** If YOLO mode is enabled and the command is non-destructive, it is logged as YOLO-approved and permitted silently.
- **Auto Mode Pre-Approval:** If YOLO is off but auto-mode is enabled under `config.json`, the extension requests an evaluation from the configured LLM. If the LLM responds with `"allow"`, execution proceeds silently.
- **Manual Dialog:** If the LLM returns anything else, times out (after `timeoutMs`, default 30s), or fails, the user is presented with a select dialog. On systems running headless without a UI (`ctx.hasUI === false`), the command is safely blocked to prevent the process from hanging.

---

## API Registrations & Integrations

The `shell-policy` extension relies on core Pi Coding Agent API integrations to enforce policy and manage session execution lifecycle.

### Pi Hook & Command Registrations

- `pi.on("tool_call", async (event, ctx) => { ... })` The core hook intercepting all tool calls (such as `bash`, `write`, `edit`) to evaluate execution modes, compare target paths, parse command structures, and prevent unauthorized actions before execution.
- `pi.registerCommand("yolo", { description, handler, ... })` Registers the `/yolo [on|off]` slash command to toggle YOLO mode.
- `pi.on("before_agent_start", async (_event, ctx) => { ... })` Checks if YOLO mode is active and injects a custom `yolo-mode-context` system message when the agent starts (only if no existing yolo context is in the current execution history block).
- `pi.on("session_start", (_event, ctx) => updateActiveTools(ctx))` Disables tools restricted by the initial execution mode at session start.
- `pi.on("before_agent_start", (_event, ctx) => updateActiveTools(ctx))` Re-applies tool restrictions right before an agent run starts to align with any dynamic mode changes.
- `pi.on("agent_end", () => { ... })` Restores the full tool lists after an agent run concludes.
- `pi.on("session_shutdown", (_event) => { ... })` Restores the registered tool lists when the session shuts down.

### Pi API Methods and Context Actions

- `pi.getActiveTools()`: Reads the currently registered and active tools.
- `pi.setActiveTools([...])`: Dynamically modifies available tools based on active execution modes.
- `pi.appendEntry(EXECUTION_MODE_ENTRY, { mode: target })`: Logs mode changes to the session entry database.
- `ctx.ui.setStatus("execution-mode", modeLabel(target))`: Updates the active status icon and text.
- `ctx.ui.notify(...)`: Notifies the user of blocks or rule applications.
- `ctx.ui.select(...)`: Prompts the user with interactive choices.
- `ctx.modelRegistry.find(provider, model)` and `ctx.modelRegistry.getApiKeyAndHeaders(model)`: Retrieves credentials and endpoints to authenticate and run the auto-mode LLM.
- `ctx.sessionManager.getBranch()` and `ctx.sessionManager.getEntries()`: Retrieves execution branch histories to parse active modes and context.
- `ctx.cwd`: References the current active workspace directory.
- `ctx.hasUI`: Boolean to detect if interactive prompts can be shown.
- `ctx.signal`: Abort signal to cancel operations or auto-mode requests.

### External Dependencies

- `complete(model, request, options)` from `@earendil-works/pi-ai`: Utilized in auto-mode to query safety and retrieve pre-approvals from configured AI models.

---

## Notable Implementation Details

### 1. Robust Tokenizer (`tokenize()`)

The policy engine relies on a strict tokenizer that processes bash command syntax according to standard shell semantics. It parses:

- Single and double quoted strings, escapes (`\`), and line continuations.
- Comments (ignores everything following `#` up to end-of-line).
- Control operators (`|`, `||`, `&&`, `;`, `&`).
- Subshells (`( ... )`), command substitutions (`$( ... )`), and backticks.
- Variable substitutions (e.g., `$VAR`, `${VAR}`). These are skipped as word fragments to prevent the agent from sneaking forbidden variables past rules.
- Redirections (`>`, `>>`, `<`, `<<`, `<<-`, `<<<`), including file descriptor prefixes (e.g., `2>`) and descriptor duplication (`>&`).
- Heredoc delimiters and bodies. Heredoc bodies are consumed to isolate them, but are not tokenized, avoiding false positives on strings or scripts passed inside heredocs.

If tokenization fails due to syntax errors (e.g., unclosed quotes), the evaluation fails closed, returning an `ask` action with a detailed parse error.

### 2. Recursive Command Unwrapping (`extractCommands`)

Before evaluating commands against policy lists, wrappers must be unpacked. The command extractor recursively handles complex wrappers using POSIX and GNU command-line standards:

- `shell-c` (e.g., `sh -c 'cmd'`): Extracts the command string following `-c` (including `--` arguments like `bash -c -- 'cmd'`).
- `utility-operand` (e.g., `sudo`, `time`): Skips standard options/flags and extracts the first non-option operand (or the token after `--`).
- `env` (e.g., `env foo=bar cmd`): Skips option flags and variable assignments to extract the nested command.
- `xargs` (e.g., `xargs cmd`): Uses operand rules to extract the target.
- `docker-run` (e.g., `docker run --rm -v /f:/b image cmd`): Parses Docker CLI flags and their expected arities, skips the image name/container ID, and extracts the trailing command payload.

### 3. Strict Policy Merging

The `mergeEvaluationPolicyStackStrict` merges `default`, active `mode`, and `project` policies sequentially under rigorous security rules:

- **Commands:** Matches from all policy layers are concatenated in priority order.
- **Redirects:** Resolves to the last defined rule in the stack, but applies the strictest action across all layers (`stricterAction`). `safeTargets` are taken from the latest policy, and `allowFdDup` is evaluated as `false` if any layer denies descriptor duplication.
- **Heredocs:** Evaluates to the strictest action encountered in the stack.
- **Wrappers:** Rules are flattened and consolidated sequentially.

### 4. Detailed Caching and Optimizations

To avoid filesystem overhead, the extension maintains in-memory maps:

- `contextTemplateCache: Map<string, string>`: Caches active mode templates.
- `promptCache: Map<string, string>`: Caches prompts for YOLO and auto-mode.
- `projectPolicyCache: Map<string, ProjectPolicyCache>`: Caches the parsed and normalized project-local `.pi/policy.json` per current working directory to prevent redundant disk reads during intensive tool loops.

### 5. Standalone CLI Debugger

Developers can run a debugger to verify evaluation behaviors locally: `nix run path:.#treefmt` or executing `scripts/evaluate-shell-policy.ts`. This CLI tool loads `policy.json`, simulates standard and mode policies, runs `analyze()` on a command argument, and outputs detailed parsing phases, token lists, segment divisions, and matched rule details in ANSI color formats.
