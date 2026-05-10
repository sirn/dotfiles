/**
 * Subagent Tool - Delegate tasks to specialized agents
 *
 * Spawns a separate `pi` process for each subagent invocation,
 * giving it an isolated context window.
 *
 * Unified `steps` schema: a 2D matrix where inner arrays run in
 * parallel and outer array runs sequentially.
 *
 *   steps: [[a, b], [c], [d, e]]
 *           └─┬─┘  └┬┘  └─┬─┘
 *          step1  step2  step3
 *           (par)  (seq)  (par)
 *
 * Single agent  = steps: [[{agent, task}]]
 * Parallel      = steps: [[t1, t2, t3]]
 * Chain         = steps: [[t1], [t2], [t3]]
 * Fanout        = steps: [[t1, t2], [t3]]
 *
 * Uses RPC mode to send tasks and capture structured output from subagents,
 * including proxied extension UI requests. Subagent system prompts
 * (persona) are surfaced back to the main agent
 */

import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { AgentToolResult } from "@mariozechner/pi-agent-core";
import type { Message } from "@mariozechner/pi-ai";
import {
  type ExtensionAPI,
  type ExtensionContext,
  type Theme,
  type ThemeColor,
  keyHint,
  getAgentDir,
  parseFrontmatter,
  getMarkdownTheme,
  withFileMutationQueue,
  DEFAULT_MAX_BYTES,
  DEFAULT_MAX_LINES,
  formatSize,
  truncateHead,
} from "@mariozechner/pi-coding-agent";
import { Container, Markdown, Spacer, Text, Box } from "@mariozechner/pi-tui";
import { Type } from "typebox";

// Cross-process protocol shared with the execution-policy extension:
// PI_EXECUTION_MODE (comma-separated stack) wins when set; otherwise the
// latest execution-mode session entry wins. Kept inline so this extension
// has no code-level dependency on execution-policy.
function readParentExecutionModes(ctx: ExtensionContext): string[] {
  const envModes = (process.env.PI_EXECUTION_MODE ?? "")
    .split(",")
    .map((m) => m.trim())
    .filter(Boolean);
  if (envModes.length > 0) return envModes;

  let mode = "edit";
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === "execution-mode") {
      const data = entry.data as { mode?: string } | undefined;
      mode = data?.mode || "edit";
    }
  }
  return [mode];
}

// ── Constants ────────────────────────────────────────────────

const MAX_CONCURRENCY = 4;
const MAX_AGENTS_PER_STEP = 8;
const COLLAPSED_ITEM_COUNT = 10;

// ── Types ────────────────────────────────────────────────────

interface AgentConfig {
  name: string;
  description: string;
  tools?: string[];
  model?: string;
  systemPrompt: string;
}

interface UsageStats {
  input: number;
  output: number;
  cacheRead: number;
  cacheWrite: number;
  cost: number;
  contextTokens: number;
  turns: number;
}

interface SingleResult {
  agent: string;
  task: string;
  exitCode: number;
  messages: Message[];
  stderr: string;
  usage: UsageStats;
  model?: string;
  stopReason?: string;
  errorMessage?: string;
  stepIndex?: number;
}

interface SubagentDetails {
  mode: "steps";
  results: SingleResult[];
  totalSteps?: number;
  totalAgents?: number;
}

type RpcEvent =
  | { type: "message_end"; message?: Message }
  | {
      type: "tool_execution_end";
      isError?: boolean;
      result?: { content?: string };
    }
  | { type: "agent_end"; messages?: Message[] }
  | {
      type: "response";
      id?: string;
      command?: string;
      success: boolean;
      error?: string;
    }
  | {
      type: "extension_ui_request";
      id: string;
      method: string;
      title?: string;
      message?: string;
      options?: string[];
      placeholder?: string;
      prefill?: string;
      notifyType?: "info" | "warning" | "error";
      statusKey?: string;
      statusText?: string;
      widgetKey?: string;
      widgetLines?: string[];
      text?: string;
    }
  | { type: string; [key: string]: unknown };

function createEmptyUsage(): UsageStats {
  return {
    input: 0,
    output: 0,
    cacheRead: 0,
    cacheWrite: 0,
    cost: 0,
    contextTokens: 0,
    turns: 0,
  };
}

function createPendingResult(
  agent: string,
  task: string,
  stepIndex?: number,
): SingleResult {
  return {
    agent,
    task,
    exitCode: -1,
    messages: [],
    stderr: "",
    usage: createEmptyUsage(),
    stepIndex,
  };
}

function createErrorResult(
  agent: string,
  task: string,
  errorMessage: string,
  exitCode = 1,
  stderr = errorMessage,
  model?: string,
  messages: Message[] = [],
): SingleResult {
  return {
    agent,
    task,
    exitCode,
    messages,
    stderr,
    usage: createEmptyUsage(),
    model,
    errorMessage,
  };
}

function isPendingResult(result: SingleResult): boolean {
  return result.exitCode === -1;
}

function isFailedResult(result: SingleResult): boolean {
  return (
    !isPendingResult(result) &&
    (result.exitCode !== 0 ||
      result.stopReason === "error" ||
      result.stopReason === "aborted")
  );
}

function getResultErrorMessage(result: SingleResult): string {
  return (
    result.errorMessage ||
    result.stderr.trim() ||
    getFinalOutput(result.messages) ||
    (result.stopReason ? `Stopped: ${result.stopReason}` : "(no output)")
  );
}

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

// ── Helpers ──────────────────────────────────────────────────

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
  themeFg: (color: ThemeColor, text: string) => string,
): string {
  const shortenPath = (p: string) => {
    const home = os.homedir();
    return p.startsWith(home) ? `~${p.slice(home.length)}` : p;
  };

  switch (toolName) {
    case "bash": {
      const command = (args.command as string) || "...";
      const preview =
        command.length > 60 ? `${command.slice(0, 60)}...` : command;
      return themeFg("muted", "$ ") + themeFg("toolOutput", preview);
    }
    case "read": {
      const rawPath = (args.file_path || args.path || "...") as string;
      const filePath = shortenPath(rawPath);
      const offset = args.offset as number | undefined;
      const limit = args.limit as number | undefined;
      let text = themeFg("accent", filePath);
      if (offset !== undefined || limit !== undefined) {
        const startLine = offset ?? 1;
        const endLine = limit !== undefined ? startLine + limit - 1 : "";
        text += themeFg(
          "warning",
          `:${startLine}${endLine ? `-${endLine}` : ""}`,
        );
      }
      return themeFg("muted", "read ") + text;
    }
    case "write": {
      const rawPath = (args.file_path || args.path || "...") as string;
      const filePath = shortenPath(rawPath);
      const content = (args.content || "") as string;
      const lines = content.split("\n").length;
      let text = themeFg("muted", "write ") + themeFg("accent", filePath);
      if (lines > 1) text += themeFg("dim", ` (${lines} lines)`);
      return text;
    }
    case "edit": {
      const rawPath = (args.file_path || args.path || "...") as string;
      return (
        themeFg("muted", "edit ") + themeFg("accent", shortenPath(rawPath))
      );
    }
    case "ls": {
      const rawPath = (args.path || ".") as string;
      return themeFg("muted", "ls ") + themeFg("accent", shortenPath(rawPath));
    }
    case "find": {
      const pattern = (args.pattern || "*") as string;
      const rawPath = (args.path || ".") as string;
      return (
        themeFg("muted", "find ") +
        themeFg("accent", pattern) +
        themeFg("dim", ` in ${shortenPath(rawPath)}`)
      );
    }
    case "grep": {
      const pattern = (args.pattern || "") as string;
      const rawPath = (args.path || ".") as string;
      return (
        themeFg("muted", "grep ") +
        themeFg("accent", `/${pattern}/`) +
        themeFg("dim", ` in ${shortenPath(rawPath)}`)
      );
    }
    default: {
      const argsStr = JSON.stringify(args);
      const preview =
        argsStr.length > 50 ? `${argsStr.slice(0, 50)}...` : argsStr;
      return themeFg("accent", toolName) + themeFg("dim", ` ${preview}`);
    }
  }
}

function getFinalOutput(messages: Message[]): string {
  for (let i = messages.length - 1; i >= 0; i--) {
    const msg = messages[i];
    if (msg.role === "assistant") {
      for (const part of msg.content) {
        if (part.type === "text") return part.text;
      }
    }
  }
  return "";
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

function truncateFinalOutput(output: string): string {
  const truncation = truncateHead(output, {
    maxLines: DEFAULT_MAX_LINES,
    maxBytes: DEFAULT_MAX_BYTES,
  });

  if (!truncation.truncated) return truncation.content;

  return [
    truncation.content,
    `\n[Subagent final answer truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines (${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)}). Ask the subagent for a narrower/shorter answer if more detail is needed.]`,
  ].join("\n");
}

type DisplayItem =
  | { type: "text"; text: string }
  | { type: "toolCall"; name: string; args: Record<string, unknown> };

function getDisplayItems(messages: Message[]): DisplayItem[] {
  const items: DisplayItem[] = [];
  for (const msg of messages) {
    if (msg.role === "assistant") {
      for (const part of msg.content) {
        if (part.type === "text") {
          if (part.text.trim()) items.push({ type: "text", text: part.text });
        } else if (part.type === "toolCall")
          items.push({
            type: "toolCall",
            name: part.name,
            args: part.arguments,
          });
      }
    }
  }
  return items;
}

async function mapWithConcurrencyLimit<TIn, TOut>(
  items: TIn[],
  concurrency: number,
  fn: (item: TIn, index: number) => Promise<TOut>,
): Promise<TOut[]> {
  if (items.length === 0) return [];
  const limit = Math.max(1, Math.min(concurrency, items.length));
  const results: TOut[] = new Array(items.length);
  let nextIndex = 0;
  const workers = new Array(limit).fill(null).map(async () => {
    while (true) {
      const current = nextIndex++;
      if (current >= items.length) return;
      results[current] = await fn(items[current], current);
    }
  });
  await Promise.all(workers);
  return results;
}

async function writePromptToTempFile(
  agentName: string,
  prompt: string,
): Promise<{ dir: string; filePath: string }> {
  const tmpDir = await fs.promises.mkdtemp(
    path.join(os.tmpdir(), "pi-subagent-"),
  );
  const safeName = agentName.replace(/[^\w.-]+/g, "_");
  const filePath = path.join(tmpDir, `prompt-${safeName}.md`);
  await withFileMutationQueue(filePath, async () => {
    await fs.promises.writeFile(filePath, prompt, {
      encoding: "utf-8",
      mode: 0o600,
    });
  });
  return { dir: tmpDir, filePath };
}

function getPiInvocation(args: string[]): { command: string; args: string[] } {
  const currentScript = process.argv[1];
  const isBunVirtualScript = currentScript?.startsWith("/$bunfs/root/");
  if (currentScript && !isBunVirtualScript && fs.existsSync(currentScript)) {
    return { command: process.execPath, args: [currentScript, ...args] };
  }

  const execName = path.basename(process.execPath).toLowerCase();
  const isGenericRuntime = /^(node|bun)(\.exe)?$/.test(execName);
  if (!isGenericRuntime) {
    return { command: process.execPath, args };
  }

  return { command: "pi", args };
}

// ── Agent Discovery ──────────────────────────────────────────

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
      parseFrontmatter<Record<string, string>>(content);
    if (!frontmatter.name || !frontmatter.description) continue;

    const tools = frontmatter.tools
      ?.split(",")
      .map((t: string) => t.trim())
      .filter(Boolean);

    agents.push({
      name: frontmatter.name,
      description: frontmatter.description,
      tools: tools && tools.length > 0 ? tools : undefined,
      model: frontmatter.model,
      systemPrompt: body,
    });
  }
  return agents;
}

// ── Core: runSingleAgent ─────────────────────────────────────

type OnUpdateCallback = (partial: AgentToolResult<SubagentDetails>) => void;

async function runSingleAgent(
  defaultCwd: string,
  agents: AgentConfig[],
  parentModes: string[],
  agentName: string,
  task: string,
  cwd: string | undefined,
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

  const args: string[] = ["--mode", "rpc", "--no-session"];
  if (agent.model) args.push("--model", agent.model);
  if (agent.tools && agent.tools.length > 0)
    args.push("--tools", agent.tools.join(","));

  let tmpPromptDir: string | null = null;
  let tmpPromptPath: string | null = null;

  const currentResult = createPendingResult(agentName, task);
  currentResult.model = agent.model;

  const emitUpdate = () => {
    if (onUpdate) {
      onUpdate({
        content: [
          {
            type: "text",
            text: getFinalOutput(currentResult.messages) || "(running...)",
          },
        ],
        details: makeDetails([currentResult]),
      });
    }
  };

  try {
    if (agent.systemPrompt.trim()) {
      const tmp = await writePromptToTempFile(agent.name, agent.systemPrompt);
      tmpPromptDir = tmp.dir;
      tmpPromptPath = tmp.filePath;
      args.push("--append-system-prompt", tmpPromptPath);
    }

    const promptCommand = {
      id: "subagent-prompt",
      type: "prompt",
      message: `Task: ${task}`,
    };
    let wasAborted = false;
    let spawnErrorMessage: string | undefined;

    const childModes = [...parentModes, "subagent", `subagent:${agent.name}`];
    const exitCode = await new Promise<number>((resolve) => {
      const invocation = getPiInvocation(args);
      const proc = spawn(invocation.command, invocation.args, {
        cwd: cwd ?? defaultCwd,
        shell: false,
        stdio: ["pipe", "pipe", "pipe"],
        env: {
          ...process.env,
          PI_EXECUTION_MODE: childModes.join(","),
        },
      });
      let buffer = "";
      let killTimeout: ReturnType<typeof setTimeout> | undefined;
      let settled = false;

      const sendRpc = (message: Record<string, unknown>) => {
        if (!proc.stdin || proc.stdin.destroyed || proc.stdin.writableEnded)
          return;
        try {
          proc.stdin.write(`${JSON.stringify(message)}\n`);
        } catch {
          /* ignore closed RPC stdin */
        }
      };

      const killProc = () => {
        wasAborted = true;
        currentResult.stopReason = "aborted";
        currentResult.errorMessage = "Subagent was aborted";
        sendRpc({ type: "abort" });
        proc.kill("SIGTERM");
        killTimeout = setTimeout(() => {
          if (!proc.killed) proc.kill("SIGKILL");
        }, 5000);
      };

      const cleanup = () => {
        if (killTimeout) clearTimeout(killTimeout);
        if (signal) signal.removeEventListener("abort", killProc);
      };

      const finish = (code: number) => {
        if (settled) return;
        settled = true;
        cleanup();
        resolve(code);
      };

      const finishSuccess = () => {
        if (proc.stdin && !proc.stdin.destroyed && !proc.stdin.writableEnded) {
          proc.stdin.end();
        }
        finish(0);
        proc.kill("SIGTERM");
      };

      let lineQueue = Promise.resolve();

      const handleExtensionUIRequest = async (
        event: Extract<RpcEvent, { type: "extension_ui_request" }>,
      ) => {
        const { id, method } = event;
        switch (method) {
          case "select": {
            const value = await ctx.ui.select(
              event.title ?? event.message ?? "Select",
              event.options ?? [],
            );
            sendRpc(
              value === undefined
                ? { type: "extension_ui_response", id, cancelled: true }
                : { type: "extension_ui_response", id, value },
            );
            break;
          }
          case "confirm": {
            const value = await ctx.ui.select(
              event.message
                ? `${event.title ?? "Confirm"}: ${event.message}`
                : (event.title ?? "Confirm"),
              ["Yes", "No"],
            );
            sendRpc(
              value === undefined
                ? { type: "extension_ui_response", id, cancelled: true }
                : {
                    type: "extension_ui_response",
                    id,
                    confirmed: value === "Yes",
                  },
            );
            break;
          }
          case "input":
          case "editor":
            sendRpc({ type: "extension_ui_response", id, cancelled: true });
            break;
          case "notify":
            if (event.message) {
              const notifyType =
                event.notifyType === "warning" || event.notifyType === "error"
                  ? event.notifyType
                  : "info";
              ctx.ui.notify(event.message, notifyType);
            }
            break;
          default:
            break;
        }
      };

      const processLine = async (line: string) => {
        if (!line.trim()) return;
        let event: RpcEvent;
        try {
          event = JSON.parse(line);
        } catch {
          return;
        }

        if (event.type === "message_end" && event.message) {
          const msg = event.message as Message;
          currentResult.messages.push(msg);

          if (msg.role === "assistant") {
            currentResult.usage.turns++;
            const usage = msg.usage;
            if (usage) {
              currentResult.usage.input += usage.input || 0;
              currentResult.usage.output += usage.output || 0;
              currentResult.usage.cacheRead += usage.cacheRead || 0;
              currentResult.usage.cacheWrite += usage.cacheWrite || 0;
              currentResult.usage.cost += usage.cost?.total || 0;
              currentResult.usage.contextTokens = usage.totalTokens || 0;
            }
            if (!currentResult.model && msg.model)
              currentResult.model = msg.model;
            if (msg.stopReason) currentResult.stopReason = msg.stopReason;
            if (msg.errorMessage) currentResult.errorMessage = msg.errorMessage;
          }
          emitUpdate();
        }

        if (event.type === "tool_execution_end" && event.isError) {
          currentResult.errorMessage ||= getResultErrorMessage({
            ...currentResult,
            messages: [],
            errorMessage:
              typeof event.result?.content === "string"
                ? event.result.content
                : undefined,
          });
        }

        if (event.type === "extension_ui_request") {
          await handleExtensionUIRequest(event);
        }

        if (event.type === "agent_end") {
          finishSuccess();
        }
      };

      const enqueueLine = (line: string) => {
        lineQueue = lineQueue.then(() => processLine(line));
        return lineQueue;
      };

      proc.stdout.on("data", (data) => {
        buffer += data.toString();
        const lines = buffer.split("\n");
        buffer = lines.pop() || "";
        for (const line of lines) enqueueLine(line);
      });

      proc.stderr.on("data", (data) => {
        currentResult.stderr += data.toString();
      });

      proc.on("close", (code, childSignal) => {
        if (buffer.trim()) enqueueLine(buffer);
        lineQueue.then(
          () => {
            if (typeof code === "number") finish(code);
            else if (wasAborted || childSignal) finish(130);
            else finish(spawnErrorMessage ? 1 : 1);
          },
          (error) => {
            currentResult.errorMessage =
              error instanceof Error ? error.message : String(error);
            currentResult.stderr += `${currentResult.errorMessage}\n`;
            finish(1);
          },
        );
      });

      proc.on("error", (error) => {
        spawnErrorMessage = `Failed to start pi for agent "${agentName}": ${error.message}`;
        currentResult.errorMessage = spawnErrorMessage;
        currentResult.stderr += `${spawnErrorMessage}\n`;
        finish(1);
      });

      sendRpc(promptCommand);

      if (signal) {
        if (signal.aborted) killProc();
        else signal.addEventListener("abort", killProc, { once: true });
      }
    });

    currentResult.exitCode = exitCode;
    if (wasAborted) {
      currentResult.stopReason = "aborted";
      currentResult.errorMessage ||= "Subagent was aborted";
      return currentResult;
    }
    if (spawnErrorMessage) currentResult.errorMessage = spawnErrorMessage;
    if (exitCode !== 0 && !currentResult.errorMessage) {
      currentResult.errorMessage = `Subagent process exited with code ${exitCode}`;
    }
    return currentResult;
  } finally {
    if (tmpPromptPath)
      try {
        fs.unlinkSync(tmpPromptPath);
      } catch {
        /* ignore */
      }
    if (tmpPromptDir)
      try {
        fs.rmdirSync(tmpPromptDir);
      } catch {
        /* ignore */
      }
  }
}

// ── Custom Message Renderers (persona surfacing) ─────────────

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
      box.addChild(new Markdown(body.trim(), 0, 0, getMarkdownTheme()));
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
      box.addChild(new Text(preview, 0, 0));
      box.addChild(new Spacer(1));
      box.addChild(
        new Text(
          theme.fg("muted", `(${keyHint("app.tools.expand", "to expand")})`),
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

// ── Tool Registration ────────────────────────────────────────

const TaskItem = Type.Object({
  agent: Type.String({ description: "Name of the agent to invoke" }),
  task: Type.String({
    description: "Task to delegate. May use {previous} in chained steps.",
  }),
  cwd: Type.Optional(
    Type.String({ description: "Working directory for the agent process" }),
  ),
});

const SubagentParams = Type.Object({
  steps: Type.Array(Type.Array(TaskItem), {
    description:
      "2D array of {agent, task}. Inner arrays run in parallel; outer runs sequentially. " +
      "Single: [[{agent, task}]]. Parallel: [[t1, t2, ...]]. Chain: [[t1], [t2], ...]. " +
      "Task may contain {previous} which is replaced with the prior step's combined output. Max 8 agents per step.",
  }),
});

export default function (pi: ExtensionAPI) {
  pi.registerMessageRenderer(
    "subagent-result-success",
    createSubagentResultRenderer("󰏫 subagent completed", "success", "Done."),
  );
  pi.registerMessageRenderer(
    "subagent-result-error",
    createSubagentResultRenderer("󰏬 subagent failed", "error", "Failed."),
  );
  pi.registerMessageRenderer(
    "subagent-result-running",
    createSubagentResultRenderer("󰏯 subagent running", "accent", "Running..."),
  );

  pi.registerTool({
    name: "subagent",
    label: "Subagent",
    description: [
      "Delegate tasks to specialized subagents with isolated context.",
      "Schema: steps: [[{agent, task}, ...], ...] — inner arrays run parallel, outer runs sequentially.",
      "Modes: single ([[{agent, task}]]), parallel ([[t1, t2]]), chain ([[t1], [t2]]), fanout ([[t1, t2], [t3]]).",
      "Subagent personas are surfaced in result messages.",
    ].join(" "),
    parameters: SubagentParams,

    async execute(_toolCallId, params, signal, onUpdate, ctx) {
      // Discover agents from user directory
      const agentDir = path.join(getAgentDir(), "agents");
      const agents = discoverAgents(agentDir);

      const totalAgents = params.steps.reduce(
        (sum, step) => sum + step.length,
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

      const parentModes = readParentExecutionModes(ctx);
      const allResults: SingleResult[] = [];
      let previousOutput = "";

      for (let stepIndex = 0; stepIndex < steps.length; stepIndex++) {
        const stepAgents = steps[stepIndex];

        // Replace {previous} in each task
        const stepTasks = stepAgents.map((a) => ({
          ...a,
          task: a.task.replace(/\{previous\}/g, previousOutput),
        }));

        const stepResults = stepTasks.map((t) =>
          createPendingResult(t.agent, t.task, stepIndex),
        );

        const emitStepUpdate = () => {
          if (onUpdate) {
            const running = stepResults.filter(isPendingResult).length;
            const done = stepResults.length - running;
            const msg =
              steps.length === 1
                ? `Step 1/1: ${done}/${stepTasks.length} done, ${running} running...`
                : `Step ${stepIndex + 1}/${steps.length}: ${done}/${stepTasks.length} done, ${running} running...`;
            onUpdate(
              createTextResult(
                msg,
                makeDetails([...allResults, ...stepResults]),
                "custom",
                "subagent-result-running",
              ),
            );
          }
        };

        await mapWithConcurrencyLimit(
          stepTasks,
          MAX_CONCURRENCY,
          async (t, i) => {
            const result = await runSingleAgent(
              ctx.cwd,
              agents,
              parentModes,
              t.agent,
              t.task,
              t.cwd,
              signal,
              (partial) => {
                if (partial.details?.results[0]) {
                  const partialResult = partial.details.results[0];
                  partialResult.stepIndex = stepIndex;
                  stepResults[i] = partialResult;
                  emitStepUpdate();
                }
              },
              makeDetails,
              ctx,
            );
            result.stepIndex = stepIndex;
            stepResults[i] = result;
            emitStepUpdate();
            return result;
          },
        );

        // Store completed step results
        allResults.push(...stepResults);

        const anyFailed = stepResults.some(isFailedResult);
        if (anyFailed) {
          const failedAgents = stepResults
            .filter(isFailedResult)
            .map((r) => r.agent)
            .join(", ");
          const errorMsg = stepResults
            .filter(isFailedResult)
            .map((r) => `[${r.agent}] ${getResultErrorMessage(r)}`)
            .join("\n");

          return createTextResult(
            `Stopped at step ${stepIndex + 1}/${steps.length} (${failedAgents}):\n${errorMsg}`,
            makeDetails(allResults),
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

      // ── All steps succeeded ─────────────────────────────

      const finalOutput = truncateFinalOutput(
        buildStepsFinalOutput(allResults),
      );

      return createTextResult(
        finalOutput,
        makeDetails(allResults),
        "custom",
        "subagent-result-success",
      );
    },

    // ── TUI: renderCall ────────────────────────────────────
    renderCall(args, theme, _context) {
      const steps = args.steps;
      if (!steps || steps.length === 0) {
        return new Text(theme.fg("muted", "subagent (no steps)"), 0, 0);
      }

      // Count total agents
      const totalAgents = steps.reduce((sum, s) => sum + s.length, 0);

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

    // ── TUI: renderResult ──────────────────────────────────
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

      const trimLines = (value: string, maxLines: number) => {
        const lines = value.trim().split("\n");
        const suffix = lines.length > maxLines ? "\n..." : "";
        return lines.slice(0, maxLines).join("\n") + suffix;
      };

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
              ? trimLines(item.text, 8)
              : trimInline(item.text, 160);
            if (!preview.trim()) continue;
            text += `${theme.fg("toolOutput", preview)}\n`;
          } else {
            text += `${theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme))}\n`;
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
          const rIcon = isPending
            ? theme.fg("warning", "")
            : hasFailed
              ? theme.fg("error", "✗")
              : theme.fg("success", "✓");
          const task = r.task
            ? trimInline(r.task, expanded ? 160 : 100)
            : undefined;
          const displayItems = getDisplayItems(r.messages);

          text += `${resultIndex > 0 ? "\n" : ""}\n  ${rIcon} ${theme.fg(
            "muted",
            `[${agentNumber}]`,
          )} ${theme.fg("accent", r.agent)}`;
          if (task)
            text += `\n    ${theme.fg("muted", "Task: ")}${theme.fg("dim", task)}`;

          if (displayItems.length === 0) {
            const fallback = hasFailed
              ? getResultErrorMessage(r)
              : isPending
                ? ""
                : "(no output)";
            if (fallback)
              text += `\n    ${theme.fg(hasFailed ? "error" : "muted", fallback)}`;
          } else {
            const rendered = renderDisplayItems(
              displayItems,
              expanded ? undefined : 3,
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
        text += `${hasTotal ? "\n" : "\n\n"}${theme.fg("muted", "(Ctrl+O to expand)")}`;
      return new Text(text, 0, 0);
    },
  });
}
