/**
 * Subagent Tool - Delegate tasks to specialized agents
 * Spawns a separate process (Pi RPC or Claude Code --print) for each
 * subagent invocation, giving it an isolated context window.
 *
 * Unified `steps` schema: a 2D matrix where inner arrays run in
 * parallel and outer array runs sequentially.
 *
 *   steps: [[a, b], [c], [d, e]]
 *           └-┬-┘  └┬┘  └-┬-┘
 *          step1  step2  step3
 *           (par)  (seq)  (par)
 *
 * Single agent  = steps: [[{agent, task}]]
 * Parallel      = steps: [[t1, t2, t3]]
 * Chain         = steps: [[t1], [t2], [t3]]
 * Fanout        = steps: [[t1, t2], [t3]]
 *
 * Uses RPC mode (Pi) or --print stream-json (Claude Code) to send tasks
 * and capture structured output from subagents, including proxied
 * extension UI requests (Pi only). Subagent system prompts (persona)
 * are surfaced back to the main agent.
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { AgentToolResult } from "@earendil-works/pi-agent-core";
import type { Message } from "@earendil-works/pi-ai";
import {
  type ExtensionAPI,
  type ExtensionContext,
  type Theme,
  keyHint,
  getAgentDir,
  parseFrontmatter,
  getMarkdownTheme,
  DEFAULT_MAX_BYTES,
  DEFAULT_MAX_LINES,
  formatSize,
  truncateHead,
} from "@earendil-works/pi-coding-agent";
import { Container, Markdown, Spacer, Text, Box } from "@earendil-works/pi-tui";
import { Type } from "typebox";

import {
  type AgentConfig,
  type SingleResult,
  type SubagentDetails,
  type OnUpdateCallback,
  type AgentRunner,
  createPendingResult,
  createErrorResult,
  isPendingResult,
  isSkippedResult,
  isFailedResult,
  getResultErrorMessage,
  getFinalOutput,
  writeOutputToTempFile,
} from "./types.js";
import { runPiAgent } from "./pi-runner.js";
import { runClaudeCodeAgent } from "./claude-code-runner.js";

// Nerd Font glyphs used in TUI rendering, collected here so the raw
// code points live in one place instead of scattered inline.
const ICONS = {
  completed: "\u{F03EB}", // subagent completed (result header)
  failed: "\u{F03EC}", // subagent failed (result header)
  running: "\u{F03EF}", // subagent running (result header)
  waiting: "\u{25CB}", // agent waiting to start
  pending: "\u{F43A}", // agent in progress
  skipped: "\u{2298}", // agent skipped
  agentFailed: "\u{F467}", // agent failed
  agentSuccess: "\u{F42E}", // agent succeeded
  compacting: "\u{F48C}", // agent compacting context
  retrying: "\u{F46A}", // agent auto-retrying
} as const;

// Runner registry: the single source of truth for which runners exist
// and how an agent's `runner` field maps to an implementation.
const RUNNERS: Record<AgentConfig["runner"], AgentRunner> = {
  pi: runPiAgent,
  "claude-code": runClaudeCodeAgent,
};

// Tool names that create or modify files, used to surface file changes.
const WRITE_TOOLS = new Set(["write", "Write", "edit", "Edit", "MultiEdit"]);

// Cross-process protocol shared with the execution-policy extension:
// PI_EXECUTION_MODE (comma-separated stack) wins when set; otherwise the
// latest execution-mode session entry wins. Kept inline so this extension
// has no code-level dependency on execution-policy.
const MODE_DELEGATE = "delegate";

// Returns the parent execution modes that should propagate to subagent
// children. Non-propagating modes (e.g. "delegate") are filtered out so
// children don't inherit restrictions meant only for the orchestrator.
function getInheritedExecutionModes(ctx: ExtensionContext): string[] {
  const envModes = (process.env.PI_EXECUTION_MODE ?? "")
    .split(",")
    .map((m) => m.trim())
    .filter(Boolean);
  if (envModes.length > 0) return envModes.filter((m) => m !== MODE_DELEGATE);

  let mode = "edit";
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === "execution-mode") {
      const data = entry.data as { mode?: string } | undefined;
      mode = data?.mode || "edit";
    }
  }
  return mode === MODE_DELEGATE ? [] : [mode];
}

// Config & Constants

interface SubagentConfig {
  maxConcurrency?: number;
  maxAgentsPerStep?: number;
  collapsedItemCount?: number;
  agentConcurrency?: Record<string, number>;
}

const DEFAULT_CONFIG: SubagentConfig = {
  maxConcurrency: 4,
  maxAgentsPerStep: 8,
  collapsedItemCount: 3,
  agentConcurrency: {},
};

function isPositiveSafeInt(v: unknown): v is number {
  return typeof v === "number" && Number.isSafeInteger(v) && v > 0;
}

function isNonNegSafeInt(v: unknown): v is number {
  return typeof v === "number" && Number.isSafeInteger(v) && v >= 0;
}

const MAX_CONCURRENCY_CEILING = 16;
const MAX_AGENTS_PER_STEP_CEILING = 32;

function loadConfig(): SubagentConfig {
  const configPath = path.join(
    os.homedir(),
    ".pi/agent/custom/subagent/config.json",
  );
  if (!fs.existsSync(configPath)) return { ...DEFAULT_CONFIG };
  try {
    const raw = fs.readFileSync(configPath, "utf-8");
    const parsed = JSON.parse(raw);
    const result = { ...DEFAULT_CONFIG };
    if (isPositiveSafeInt(parsed.maxConcurrency))
      result.maxConcurrency = Math.min(
        parsed.maxConcurrency,
        MAX_CONCURRENCY_CEILING,
      );
    if (isPositiveSafeInt(parsed.maxAgentsPerStep))
      result.maxAgentsPerStep = Math.min(
        parsed.maxAgentsPerStep,
        MAX_AGENTS_PER_STEP_CEILING,
      );
    if (isPositiveSafeInt(parsed.collapsedItemCount))
      result.collapsedItemCount = parsed.collapsedItemCount;
    if (
      typeof parsed.agentConcurrency === "object" &&
      parsed.agentConcurrency !== null
    ) {
      const ac: Record<string, number> = {};
      for (const [k, v] of Object.entries(parsed.agentConcurrency)) {
        if (isNonNegSafeInt(v)) ac[k] = Math.min(v, MAX_CONCURRENCY_CEILING);
      }
      result.agentConcurrency = ac;
    }
    return result;
  } catch {
    return { ...DEFAULT_CONFIG };
  }
}

const subagentConfig = loadConfig();
const MAX_CONCURRENCY = subagentConfig.maxConcurrency ?? 4;
const MAX_AGENTS_PER_STEP = subagentConfig.maxAgentsPerStep ?? 8;
const COLLAPSED_ITEM_COUNT = subagentConfig.collapsedItemCount ?? 3;

// Cost restored from previous session history on startup, so
// setSubagentCost adds to it instead of overwriting it.
let sessionRestoredCost = 0;
function createTextResult(
  text: string,
  details: SubagentDetails,
  type: "text" | "custom" = "text",
  customType?: string,
  extra: Record<string, unknown> = {},
): AgentToolResult<SubagentDetails> {
  const content =
    type === "custom"
      ? [
          { type: "text", text },
          { type, customType, content: text, ...extra },
        ]
      : [{ type, text }];

  return { content, details };
}

function setSubagentCost(ctx: ExtensionContext, results: SingleResult[]): void {
  const currentCost = results
    .filter((r) => !isPendingResult(r) && !isSkippedResult(r))
    .reduce((sum, r) => sum + r.usage.cost, 0);
  sessionRestoredCost += currentCost;
  ctx.ui.setStatus(
    "subagent-cost",
    sessionRestoredCost > 0 ? `+$${sessionRestoredCost.toFixed(2)}` : undefined,
  );
}

function restoreSubagentCost(ctx: ExtensionContext): void {
  const branch = ctx.sessionManager.getBranch() as {
    type: string;
    message?: { role: string; details?: unknown };
  }[];
  let restoredCost = 0;
  for (const entry of branch) {
    if (entry.type === "message" && entry.message?.role === "toolResult") {
      const details = entry.message.details as SubagentDetails | undefined;
      if (details?.mode === "steps") {
        for (const result of details.results ?? []) {
          restoredCost += result.usage?.cost ?? 0;
        }
      }
    }
  }
  sessionRestoredCost = restoredCost;
  if (restoredCost > 0) {
    ctx.ui.setStatus("subagent-cost", `+$${restoredCost.toFixed(2)}`);
  }
}

// Helpers

function formatTokens(count: number): string {
  if (count < 1000) return count.toString();
  if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
  if (count < 1000000) return `${Math.round(count / 1000)}k`;
  return `${(count / 1000000).toFixed(1)}M`;
}

function formatUsageStats(
  usage: {
    input: number;
    output: number;
    cacheRead: number;
    cacheWrite: number;
    cost: number;
    contextTokens?: number;
    turns?: number;
  },
  model?: string,
): string {
  const parts: string[] = [];
  if (usage.turns)
    parts.push(`${usage.turns} turn${usage.turns > 1 ? "s" : ""}`);
  if (usage.input) parts.push(`↑${formatTokens(usage.input)}`);
  if (usage.output) parts.push(`↓${formatTokens(usage.output)}`);
  if (usage.cacheRead) parts.push(`R${formatTokens(usage.cacheRead)}`);
  if (usage.cacheWrite) parts.push(`W${formatTokens(usage.cacheWrite)}`);
  if (usage.cost) parts.push(`$${usage.cost.toFixed(4)}`);
  if (usage.contextTokens && usage.contextTokens > 0) {
    parts.push(`ctx:${formatTokens(usage.contextTokens)}`);
  }
  if (model) parts.push(model);
  return parts.join(" ");
}

function formatToolCall(
  toolName: string,
  args: Record<string, unknown>,
): { name: string; arg?: string } {
  const arg: string | undefined =
    typeof args.command === "string"
      ? args.command
      : typeof args.file_path === "string"
        ? args.file_path
        : typeof args.path === "string"
          ? args.path
          : typeof args.pattern === "string"
            ? args.pattern
            : typeof args.url === "string"
              ? args.url
              : undefined;
  return { name: toolName, arg };
}

function formatFinalOutput(result: SingleResult): string {
  return getFinalOutput(result.messages) || "(no output)";
}

function formatStepHeading(stepIndex: number, result: SingleResult): string {
  return `Step ${stepIndex + 1} [${result.agent}]`;
}

function buildStepsFinalOutput(results: SingleResult[]): string {
  if (results.length === 0) return "(no output)";
  if (results.length === 1) return formatFinalOutput(results[0]);

  return results
    .map((result) => {
      const stepIndex = result.stepIndex ?? 0;
      return `${formatStepHeading(stepIndex, result)}\n${formatFinalOutput(result)}`;
    })
    .join("\n\n");
}

function extractFileChanges(results: SingleResult[]): string[] {
  const paths = new Set<string>();
  for (const result of results) {
    for (const msg of result.messages) {
      if (msg.role !== "assistant") continue;
      for (const part of msg.content) {
        if (
          part.type === "toolCall" &&
          part.name &&
          WRITE_TOOLS.has(part.name)
        ) {
          const args = part.arguments;
          if (!args || typeof args !== "object") continue;
          const filePath =
            (args as Record<string, unknown>).file_path ||
            (args as Record<string, unknown>).path ||
            (args as Record<string, unknown>).filePath;
          if (typeof filePath === "string" && filePath.trim()) {
            paths.add(filePath.trim());
          }
        }
      }
    }
  }
  return [...paths].sort();
}

type DisplayItem =
  | { type: "text"; text: string }
  | { type: "toolCall"; name: string; args: Record<string, unknown> }
  | { type: "toolResult"; toolName: string; text: string; isError: boolean };

function getDisplayItems(messages: Message[]): DisplayItem[] {
  const items: DisplayItem[] = [];
  for (const msg of messages) {
    if (msg.role === "assistant") {
      for (const part of msg.content) {
        if (part.type === "text") {
          if (part.text.trim()) items.push({ type: "text", text: part.text });
        } else if (part.type === "toolCall" && part.name)
          items.push({
            type: "toolCall",
            name: part.name,
            args: part.arguments,
          });
      }
    } else if (msg.role === "toolResult") {
      const { toolName = "", isError = false } = msg as {
        toolName?: string;
        isError?: boolean;
      };
      const text = msg.content
        .filter((p: any) => p.type === "text")
        .map((p: any) => p.text)
        .join("\n");
      if (text.trim()) {
        items.push({ type: "toolResult", toolName, text, isError });
      }
    }
  }
  return items;
}

async function mapWithAgentConcurrency<TIn, TOut>(
  items: TIn[],
  globalConcurrency: number,
  getAgentName: (item: TIn) => string,
  agentConcurrencyMap: Map<string, number>,
  fn: (item: TIn, index: number) => Promise<TOut>,
): Promise<TOut[]> {
  if (items.length === 0) return [];

  const limit = Math.max(1, Math.min(globalConcurrency, items.length));
  const results: TOut[] = new Array(items.length);
  let nextIndex = 0;

  const agentSems = new Map<
    string,
    { running: number; waitQueue: (() => void)[] }
  >();
  for (const [name, max] of agentConcurrencyMap) {
    agentSems.set(name, { running: 0, waitQueue: [] });
  }

  const acquireAgent = async (agentName: string): Promise<void> => {
    const sem = agentSems.get(agentName);
    if (!sem) return;
    if (sem.running < (agentConcurrencyMap.get(agentName) ?? Infinity)) {
      sem.running++;
      return;
    }
    await new Promise<void>((resolve) => sem.waitQueue.push(resolve));
  };

  const releaseAgent = (agentName: string) => {
    const sem = agentSems.get(agentName);
    if (!sem) return;
    if (sem.waitQueue.length > 0) {
      sem.waitQueue.shift()!();
    } else {
      sem.running--;
    }
  };

  const workers = new Array(limit).fill(null).map(async () => {
    while (true) {
      const current = nextIndex++;
      if (current >= items.length) return;
      const agentName = getAgentName(items[current]);
      await acquireAgent(agentName);
      try {
        results[current] = await fn(items[current], current);
      } finally {
        releaseAgent(agentName);
      }
    }
  });

  await Promise.all(workers);
  return results;
}

// Agent Discovery

function discoverAgents(agentDir: string): AgentConfig[] {
  if (!fs.existsSync(agentDir)) return [];

  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(agentDir, { withFileTypes: true });
  } catch {
    return [];
  }

  const agents: AgentConfig[] = [];
  for (const entry of entries) {
    if (!entry.name.endsWith(".md")) continue;
    if (!entry.isFile() && !entry.isSymbolicLink()) continue;

    const filePath = path.join(agentDir, entry.name);
    let content: string;
    try {
      content = fs.readFileSync(filePath, "utf-8");
    } catch {
      continue;
    }

    const { frontmatter, body } =
      parseFrontmatter<Record<string, unknown>>(content);
    if (!frontmatter.name || !frontmatter.description) continue;

    const tools =
      typeof frontmatter.tools === "string"
        ? frontmatter.tools
            .split(",")
            .map((t: string) => t.trim())
            .filter(Boolean)
        : undefined;

    const runner: AgentConfig["runner"] =
      typeof frontmatter.runner === "string" && frontmatter.runner in RUNNERS
        ? (frontmatter.runner as AgentConfig["runner"])
        : "pi";

    agents.push({
      name: String(frontmatter.name),
      description: String(frontmatter.description),
      tools: tools && tools.length > 0 ? tools : undefined,
      model:
        typeof frontmatter.model === "string" ? frontmatter.model : undefined,
      concurrency:
        typeof frontmatter.concurrency === "number" &&
        isPositiveSafeInt(frontmatter.concurrency)
          ? frontmatter.concurrency
          : undefined,
      mode:
        typeof frontmatter.mode === "string" && frontmatter.mode.trim()
          ? frontmatter.mode.trim()
          : undefined,
      systemPrompt: body,
      runner,
    });
  }

  return agents;
}

// Core: runSingleAgent (dispatcher)

async function runSingleAgent(
  defaultCwd: string,
  agents: AgentConfig[],
  parentModes: string[],
  agentName: string,
  task: string,
  cwd: string | undefined,
  sessionId: string | undefined,
  signal: AbortSignal | undefined,
  onUpdate: OnUpdateCallback | undefined,
  makeDetails: (results: SingleResult[]) => SubagentDetails,
  ctx: ExtensionContext,
): Promise<SingleResult> {
  const agent = agents.find((a) => a.name === agentName);

  if (!agent) {
    const available = agents.map((a) => `"${a.name}"`).join(", ") || "none";
    const errorMessage = `Unknown agent: "${agentName}". Available agents: ${available}.`;
    return createErrorResult(agentName, task, errorMessage);
  }

  const runner = RUNNERS[agent.runner];

  return runner(
    agent,
    task,
    cwd ?? defaultCwd,
    parentModes,
    agent.mode,
    signal,
    onUpdate,
    makeDetails,
    sessionId,
    ctx,
  );
}

// Custom Message Renderers (persona surfacing)

function createSubagentResultRenderer(
  header: string,
  colorKey: "accent" | "success" | "error",
  fallback: string,
) {
  return (
    message: Record<string, unknown>,
    { expanded }: { expanded: boolean },
    theme: Theme,
  ) => {
    const container = new Container();
    const box = new Box(1, 1, (s: string) => theme.bg("customMessageBg", s));

    if (expanded) {
      box.addChild(new Text(theme.fg(colorKey, theme.bold(header)), 0, 0));
      box.addChild(new Spacer(1));

      // Agent persona header
      if (message.agentName) {
        box.addChild(
          new Text(
            theme.fg("accent", `Agent: ${message.agentName}`) +
              (message.agentDescription
                ? theme.fg("dim", ` — ${message.agentDescription}`)
                : ""),
            0,
            0,
          ),
        );
        box.addChild(new Spacer(1));
      }

      const body =
        typeof message.content === "string" ? message.content : fallback;
      box.addChild(
        new Markdown(body.trim(), 0, 0, getMarkdownTheme(), {
          color: (text: string) => theme.fg("customMessageText", text),
        }),
      );
    } else {
      box.addChild(new Text(theme.fg(colorKey, theme.bold(header)), 0, 0));
      box.addChild(new Spacer(1));

      if (message.agentName) {
        box.addChild(
          new Text(
            theme.fg("accent", String(message.agentName)) +
              (message.agentDescription
                ? theme.fg("dim", ` — ${message.agentDescription}`)
                : ""),
            0,
            0,
          ),
        );
        box.addChild(new Spacer(1));
      }

      const preview =
        typeof message.content === "string"
          ? message.content.split("\n").slice(0, 5).join("\n")
          : fallback;
      box.addChild(new Text(theme.fg("customMessageText", preview), 0, 0));
      box.addChild(new Spacer(1));
      box.addChild(
        new Text(
          `${theme.fg("muted", "(")}${keyHint("app.tools.expand", "to expand")}${theme.fg("muted", ")")}`,
          0,
          0,
        ),
      );
    }

    container.addChild(box);

    return {
      render: (width: number) => container.render(width),
      invalidate: () => container.invalidate(),
    };
  };
}

// Tool Registration

// Discover agents once at registration time so their names and
// descriptions can be baked into the tool schema as a hint to
// the LLM. Agents are re-discovered on each execute() call as
// well, so validation stays current if agents change.
//
// A try/catch guards against unexpected errors in getAgentDir()
// or discoverAgents (e.g. parseFrontmatter throw); on failure
// the schema falls back to a plain description with no agent list.
const discoveredAgents: AgentConfig[] = (() => {
  try {
    return discoverAgents(path.join(getAgentDir(), "agents"));
  } catch {
    return [];
  }
})();

function buildAgentHint(agents: AgentConfig[]): string {
  if (agents.length === 0)
    return "Name of the agent to invoke. No agents discovered at startup — run /reload after adding agent files.";
  const maxDescLen = 80; // cap per-agent description for LLM token budget
  const maxAgents = 12; // cap total agents shown in schema hint
  const entries = agents
    .slice(0, maxAgents)
    .map((a) => {
      const desc =
        a.description.length > maxDescLen
          ? `${a.description.slice(0, maxDescLen - 1)}…`
          : a.description;
      const runner = a.runner === "claude-code" ? " [CC]" : "";
      return `"${a.name}"${runner} — ${desc}`;
    })
    .join("; ");
  const suffix =
    agents.length > maxAgents ? `; and ${agents.length - maxAgents} more` : "";
  return `Available agents: ${entries}${suffix}.`;
}

const agentHint = buildAgentHint(discoveredAgents);

const TaskItem = Type.Object({
  agent: Type.String({ description: agentHint }),
  task: Type.String({
    description: "Task to delegate. May use {previous} in chained steps.",
  }),
  cwd: Type.Optional(
    Type.String({ description: "Working directory for the agent process" }),
  ),
  sessionId: Type.Optional(
    Type.String({
      description: "Session ID to resume. Omit to start a new session.",
    }),
  ),
});

const STEPS_DESCRIPTION =
  "2D array of {agent, task}. Inner arrays run in parallel; outer runs sequentially. " +
  "Single: [[{agent, task}]]. Parallel: [[t1, t2, ...]]. Chain: [[t1], [t2], ...]. " +
  `Task may contain {previous} which is replaced with the prior step's combined output. Max ${MAX_AGENTS_PER_STEP} agents per step.`;

const SubagentParams = Type.Object({
  steps: Type.Array(Type.Array(TaskItem), {
    description: STEPS_DESCRIPTION,
  }),
});

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.hasUI) {
      restoreSubagentCost(ctx);
    }
  });

  pi.on("session_shutdown", (_event, ctx) => {
    if (ctx.hasUI) ctx.ui.setStatus("subagent-cost", undefined);
  });

  pi.registerMessageRenderer(
    "subagent-result-success",
    createSubagentResultRenderer(
      `${ICONS.completed} subagent completed`,
      "success",
      "Done.",
    ),
  );

  pi.registerMessageRenderer(
    "subagent-result-error",
    createSubagentResultRenderer(
      `${ICONS.failed} subagent failed`,
      "error",
      "Failed.",
    ),
  );

  pi.registerMessageRenderer(
    "subagent-result-running",
    createSubagentResultRenderer(
      `${ICONS.running} subagent running`,
      "accent",
      "Running...",
    ),
  );

  pi.registerTool({
    name: "subagent",
    label: "Subagent",
    description: [
      "Delegate tasks to specialized subagents with isolated context.",
      "Supports both Pi and Claude Code runners.",
      "Schema: steps: [[{agent, task}, ...], ...] — inner arrays run parallel, outer runs sequentially.",
      "Modes: single ([[{agent, task}]]), parallel ([[t1, t2]]), chain ([[t1], [t2]]), fanout ([[t1, t2], [t3]]).",
      "Subagent personas are surfaced in result messages.",
    ].join(" "),
    parameters: SubagentParams,

    async execute(_toolCallId, params, signal, onUpdate, ctx) {
      // Discover agents from user directory
      const agentDir = path.join(getAgentDir(), "agents");
      const agents = discoverAgents(agentDir);

      // Build per-agent concurrency map: frontmatter first, config overrides
      const agentConcurrencyMap = new Map<string, number>();
      for (const agent of agents) {
        if (agent.concurrency !== undefined && agent.concurrency > 0) {
          agentConcurrencyMap.set(agent.name, agent.concurrency);
        }
      }
      for (const [name, limit] of Object.entries(
        subagentConfig.agentConcurrency ?? {},
      )) {
        if (limit > 0) agentConcurrencyMap.set(name, limit);
        else agentConcurrencyMap.delete(name); // 0 clears frontmatter limit
      }

      const totalAgents = params.steps.reduce(
        (sum, step) => sum + (Array.isArray(step) ? step.length : 0),
        0,
      );
      const makeDetails = (results: SingleResult[]): SubagentDetails => ({
        mode: "steps",
        results,
        totalSteps: params.steps.length,
        totalAgents,
      });

      const steps = params.steps;
      if (!steps || steps.length === 0) {
        return createTextResult(
          "No steps provided. Provide a 2D array: [[{agent, task}, ...], ...]",
          makeDetails([]),
        );
      }

      for (let s = 0; s < steps.length; s++) {
        if (!Array.isArray(steps[s])) {
          return createTextResult(
            `Step ${s + 1} is not an array of agent tasks. Each step must be an array like [{agent: "name", task: "description"}, ...].`,
            makeDetails([]),
          );
        }
        if (steps[s].length === 0) {
          return createTextResult(
            `Step ${s + 1} has no agents. Provide at least one agent per step.`,
            makeDetails([]),
          );
        }
        if (steps[s].length > MAX_AGENTS_PER_STEP) {
          return createTextResult(
            `Step ${s + 1} has ${steps[s].length} agents. Max ${MAX_AGENTS_PER_STEP} per step.`,
            makeDetails([]),
          );
        }
      }

      const parentModes = getInheritedExecutionModes(ctx);
      let previousOutput = "";

      // Pre-populate pending results for ALL steps so the TUI shows
      // the full plan from the start, with future steps as "waiting".
      const planResults: SingleResult[] = [];
      for (let si = 0; si < steps.length; si++) {
        for (const t of steps[si]) {
          planResults.push(createPendingResult(t.agent, t.task, si));
        }
      }

      let stepStartIndex = 0;

      for (let stepIndex = 0; stepIndex < steps.length; stepIndex++) {
        const stepAgents = steps[stepIndex];

        // Replace {previous} in each task
        const stepTasks = stepAgents.map((a) => ({
          ...a,
          task: a.task.replace(/\{previous\}/g, previousOutput),
        }));

        // Update task text for this step's agents (replacing {previous})
        for (let i = 0; i < stepTasks.length; i++) {
          planResults[stepStartIndex + i].task = stepTasks[i].task;
        }

        const emitStepUpdate = () => {
          if (onUpdate) {
            const currentStepPending = planResults.filter(
              (r) => r.stepIndex === stepIndex && isPendingResult(r),
            ).length;
            const currentStepRunning = planResults.filter(
              (r) =>
                r.stepIndex === stepIndex && isPendingResult(r) && r.started,
            ).length;
            const currentStepDone = stepTasks.length - currentStepPending;
            const msg =
              steps.length === 1
                ? `Step 1/1: ${currentStepDone}/${stepTasks.length} done, ${currentStepRunning} running...`
                : `Step ${stepIndex + 1}/${steps.length}: ${currentStepDone}/${stepTasks.length} done, ${currentStepRunning} running...`;
            onUpdate(
              createTextResult(
                msg,
                makeDetails([...planResults]),
                "custom",
                "subagent-result-running",
              ),
            );
          }
        };

        // Emit initial state so waiting steps are visible from the start
        emitStepUpdate();

        await mapWithAgentConcurrency(
          stepTasks,
          MAX_CONCURRENCY,
          (t) => t.agent,
          agentConcurrencyMap,
          async (t, i) => {
            // Mark as started when the concurrency limiter actually begins execution
            planResults[stepStartIndex + i].started = true;
            emitStepUpdate();
            const result = await runSingleAgent(
              ctx.cwd,
              agents,
              parentModes,
              t.agent,
              t.task,
              t.cwd,
              t.sessionId,
              signal,
              (partial) => {
                if (partial.details?.results[0]) {
                  const partialResult = partial.details.results[0];
                  partialResult.stepIndex = stepIndex;
                  partialResult.started = true;
                  planResults[stepStartIndex + i] = partialResult;
                  emitStepUpdate();
                }
              },
              makeDetails,
              ctx,
            );
            result.stepIndex = stepIndex;
            result.started = true;
            planResults[stepStartIndex + i] = result;
            emitStepUpdate();
            return result;
          },
        );

        const stepResults = planResults.slice(
          stepStartIndex,
          stepStartIndex + stepTasks.length,
        );

        stepStartIndex += stepTasks.length;

        const sessionIds = planResults
          .filter((r) => r.sessionId)
          .map((r) => `- ${r.agent}: ${r.sessionId}`);
        const sessionIdsText =
          sessionIds.length > 0
            ? `\n<output-meta>\n## Session IDs\n${sessionIds.join("\n")}\n</output-meta>`
            : "";

        const anyFailed = stepResults.some(isFailedResult);
        if (anyFailed) {
          const failedAgents = stepResults
            .filter(isFailedResult)
            .map((r) => r.agent)
            .join(", ");
          const errorMsg =
            stepResults
              .filter(isFailedResult)
              .map((r) => `[${r.agent}] ${getResultErrorMessage(r)}`)
              .join("\n") + sessionIdsText;

          // Mark future steps as skipped so the renderer doesn't treat them as running
          for (let si = stepStartIndex; si < planResults.length; si++) {
            if (isPendingResult(planResults[si])) {
              planResults[si].exitCode = 1;
              planResults[si].stopReason = "skipped";
              planResults[si].errorMessage = "Skipped: earlier step failed";
            }
          }

          setSubagentCost(ctx, planResults);
          return createTextResult(
            `Stopped at step ${stepIndex + 1}/${steps.length} (${failedAgents}):\n${errorMsg}`,
            makeDetails([...planResults]),
            "custom",
            "subagent-result-error",
          );
        }

        // Build combined output for next step's {previous}
        previousOutput = stepResults
          .map(
            (r) =>
              `[${r.agent}]\n${getFinalOutput(r.messages) || "(no output)"}`,
          )
          .join("\n\n");
      }

      // - All steps succeeded

      setSubagentCost(ctx, planResults);
      const fullOutput = buildStepsFinalOutput(planResults);
      const truncation = truncateHead(fullOutput, {
        maxLines: DEFAULT_MAX_LINES,
        maxBytes: DEFAULT_MAX_BYTES,
      });

      const fileChanges = extractFileChanges(planResults);
      const fileChangesText =
        fileChanges.length > 0
          ? `\n<output-meta>\n## Files changed\n${fileChanges.map((p) => "- `" + p + "`").join("\n")}\n</output-meta>`
          : "";

      const sessionIds = planResults
        .filter((r) => r.sessionId)
        .map((r) => `- ${r.agent}: ${r.sessionId}`);
      const sessionIdsText =
        sessionIds.length > 0
          ? `\n<output-meta>\n## Session IDs\n${sessionIds.join("\n")}\n</output-meta>`
          : "";

      let finalOutput: string;
      if (truncation.truncated) {
        let tmpPath: string | null = null;
        try {
          tmpPath = await writeOutputToTempFile(fullOutput);
        } catch {
          // Fall through to generic truncation notice
        }
        const truncatedBy =
          truncation.truncatedBy === "bytes"
            ? `output truncated (${formatSize(truncation.maxBytes)})`
            : `output truncated (${truncation.maxLines} lines)`;
        const truncationNotice = tmpPath
          ? `${truncatedBy}, read: ${tmpPath} for full text`
          : `[Subagent final answer truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines (${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)}). Ask the subagent for a narrower/shorter answer if more detail is needed.]`;
        finalOutput =
          truncation.content +
          "\n" +
          truncationNotice +
          fileChangesText +
          sessionIdsText;
      } else {
        finalOutput = fullOutput + fileChangesText + sessionIdsText;
      }

      return createTextResult(
        finalOutput,
        makeDetails([...planResults]),
        "custom",
        "subagent-result-success",
      );
    },

    // - TUI: renderCall
    renderCall(args, theme, _context) {
      const steps = args.steps;
      if (!steps || steps.length === 0) {
        return new Text(theme.fg("muted", "subagent (no steps)"), 0, 0);
      }

      // Count total agents
      const totalAgents = steps.reduce(
        (sum, s) => sum + (Array.isArray(s) ? s.length : 0),
        0,
      );

      const text =
        theme.fg("toolTitle", theme.bold("subagent ")) +
        theme.fg(
          "accent",
          `${steps.length} step${steps.length !== 1 ? "s" : ""}`,
        ) +
        theme.fg(
          "muted",
          ` (${totalAgents} agent${totalAgents !== 1 ? "s" : ""})`,
        );

      return new Text(text, 0, 0);
    },

    // - TUI: renderResult
    renderResult(result, { expanded }, theme, _context) {
      const details = result.details as SubagentDetails | undefined;
      if (!details || details.results.length === 0) {
        const text = result.content[0];
        return new Text(
          text?.type === "text" ? text.text : "(no output)",
          0,
          0,
        );
      }

      const trimInline = (value: string, maxLength: number) => {
        const compact = value.trim().replace(/\s+/g, " ");
        return compact.length > maxLength
          ? `${compact.slice(0, maxLength - 1)}…`
          : compact;
      };

      const renderDisplayItems = (items: DisplayItem[], limit?: number) => {
        const toShow = limit ? items.slice(-limit) : items;
        const skipped =
          limit && items.length > limit ? items.length - limit : 0;
        let text = "";
        if (skipped > 0)
          text += theme.fg(
            "muted",
            `… ${skipped} earlier display items hidden\n`,
          );
        for (const item of toShow) {
          if (item.type === "text") {
            const preview = expanded
              ? item.text.trim()
              : trimInline(item.text, 160);
            if (!preview.trim()) continue;
            text += `${theme.fg("toolOutput", preview)}\n`;
          } else if (item.type === "toolCall") {
            let toolName: string;
            let argText = "";
            try {
              const { name, arg } = formatToolCall(item.name, item.args);
              toolName = name;
              if (arg) {
                argText = ` ${theme.fg("toolOutput", expanded ? arg.trim() : trimInline(arg, 140))}`;
              }
            } catch {
              toolName = item.name;
            }
            text += `${theme.fg("muted", "→ ")}${theme.fg("accent", toolName)}${argText}\n`;
          } else if (item.type === "toolResult") {
            const prefix = item.isError
              ? theme.fg("error", "← error:")
              : theme.fg("muted", "← output:");
            const resultPreview = expanded
              ? item.text.trim()
              : trimInline(item.text, 160);
            text += `${prefix} ${theme.fg("toolOutput", resultPreview)}\n`;
          }
        }
        return text.trimEnd();
      };

      const aggregateUsage = (results: SingleResult[]) => {
        const total = {
          input: 0,
          output: 0,
          cacheRead: 0,
          cacheWrite: 0,
          cost: 0,
          turns: 0,
        };
        for (const r of results) {
          total.input += r.usage.input;
          total.output += r.usage.output;
          total.cacheRead += r.usage.cacheRead;
          total.cacheWrite += r.usage.cacheWrite;
          total.cost += r.usage.cost;
          total.turns += r.usage.turns;
        }
        return total;
      };

      const isRunning = details.results.some(isPendingResult);

      let text = "";

      const groupedResults = new Map<number, SingleResult[]>();
      for (const r of details.results) {
        const stepIndex = r.stepIndex ?? 0;
        const group = groupedResults.get(stepIndex);
        if (group) group.push(r);
        else groupedResults.set(stepIndex, [r]);
      }

      let agentNumber = 1;
      for (const [stepIndex, results] of [...groupedResults.entries()].sort(
        ([a], [b]) => a - b,
      )) {
        text += `${text ? "\n\n" : ""}${theme.fg("muted", `step ${stepIndex + 1}:`)}`;

        for (const [resultIndex, r] of results.entries()) {
          const isPending = isPendingResult(r);
          const hasFailed = isFailedResult(r);
          const isSkipped = isSkippedResult(r);
          const isWaiting = isPending && !r.started;
          const rIcon = isWaiting
            ? theme.fg("dim", ICONS.waiting)
            : isPending
              ? theme.fg("warning", ICONS.pending)
              : isSkipped
                ? theme.fg("dim", ICONS.skipped)
                : hasFailed
                  ? theme.fg("error", ICONS.agentFailed)
                  : theme.fg("success", ICONS.agentSuccess);
          const task = r.task
            ? expanded
              ? r.task.trim().replace(/\s+/g, " ")
              : trimInline(r.task, 100)
            : undefined;
          const displayItems = getDisplayItems(r.messages);

          const runnerTag =
            r.runner === "claude-code" ? theme.fg("dim", " [claude-code]") : "";
          const statusGlyph =
            isPending && (r.autoRetrying || r.compacting)
              ? theme.fg(
                  "warning",
                  r.compacting ? ICONS.compacting : ICONS.retrying,
                )
              : "";
          const resumedTag = r.sessionId
            ? theme.fg("accent", ` (resumed: ${r.sessionId})`)
            : "";
          text += `${resultIndex > 0 ? "\n" : ""}\n  ${rIcon} ${theme.fg(
            "muted",
            `[${agentNumber}]`,
          )} ${theme.fg("accent", r.agent)}${runnerTag}${resumedTag}${statusGlyph}`;
          if (task)
            text += `\n    ${theme.fg("muted", "Task: ")}${theme.fg("dim", task)}`;

          if (displayItems.length === 0) {
            const fallback = isSkipped
              ? "Skipped: earlier step failed"
              : hasFailed
                ? getResultErrorMessage(r)
                : isPending
                  ? ""
                  : "(no output)";
            if (fallback)
              text += `\n    ${theme.fg(isSkipped ? "muted" : hasFailed ? "error" : "muted", fallback)}`;
          } else {
            const rendered = renderDisplayItems(
              displayItems,
              expanded ? undefined : COLLAPSED_ITEM_COUNT,
            );
            if (rendered)
              text += `\n${rendered
                .split("\n")
                .map((line) => `    ${line}`)
                .join("\n")}`;
          }

          if (!isPending) {
            const taskUsage = formatUsageStats(r.usage, r.model);
            if (taskUsage) text += `\n    ${theme.fg("dim", taskUsage)}`;
          }

          agentNumber++;
        }
      }

      let hasTotal = false;
      if (!isRunning) {
        const usageStr = formatUsageStats(aggregateUsage(details.results));
        if (usageStr) {
          text += `\n\n${theme.fg("dim", `Total: ${usageStr}`)}`;
          hasTotal = true;
        }
      }
      if (!expanded)
        text += `${hasTotal ? "\n" : "\n\n"}${theme.fg("muted", "(")}${keyHint("app.tools.expand", "to expand")}${theme.fg("muted", ")")}`;
      return new Text(text, 0, 0);
    },
  });
}
