/**
 * Shell Policy Extension for Pi Coding Agent
 *
 * Loads command policies from policy.json and enforces them via tool_call hooks.
 * - allow: commands permitted without confirmation
 * - ask: commands requiring user confirmation
 * - deny: commands that are blocked entirely
 *
 * Policy: Ask by default - any command not explicitly allowed or denied requires confirmation.
 * Per-project overrides can be placed in .pi/policy.json relative to the project root.
 *
 * Commands that trigger ask/default are logged to ~/.pi/agent/logs/execution-policy/commands.log
 *
 * Auto mode: when policyAutoMode is enabled in custom/execution-policy/config.json, commands that would
 * normally require user confirmation are first evaluated by a small LLM using
 * auto-mode/prompt.md. If the model returns "allow", the command runs without
 * prompting the user. Any other outcome falls back to human confirmation.
 */

import { complete } from "@earendil-works/pi-ai";
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { getExecutionMode, setModeChangeHook } from "./lib/execution-mode.js";
import { EXT_DIR, PI_AGENT_DIR } from "./lib/paths.js";
import * as fs from "node:fs";
import * as path from "node:path";
import {
  evaluate,
  mergePoliciesStrict,
  mergeEvaluationPolicyStackStrict,
  normalizeUnifiedPolicyConfig,
  normalizeShellPolicyConfig,
  getCommandSummary,
  computeDisabledTools,

  type EvalResult,
  type EvaluationPolicy,
  type ModePolicy,
  type PolicyCommands,
  type WrapperRuleConfig,
} from "./lib/shell-policy.js";

function resolveModeContextPath(mode: string): string | null {
  if (mode.length > 0 && !mode.includes("/") && !mode.includes("..")) {
    return `auto-mode/${mode.replace(/:/g, ".")}.md`;
  }
  return null;
}

// Log commands that require confirmation for later policy review
const COMMANDS_LOG_DIR = path.join(PI_AGENT_DIR, "logs/execution-policy");

function logConfirmNeeded(command: string, result: EvalResult): void {
  try {
    const entry = {
      ts: new Date().toISOString(),
      command: command,
      decidedBy: result.decidedBy,
      match: result.match
        ? { [result.match.category]: result.match.entry.match }
        : undefined,
    };
    fs.mkdirSync(COMMANDS_LOG_DIR, { recursive: true });
    // Enforce restrictive perms even if dir/file already existed
    fs.chmodSync(COMMANDS_LOG_DIR, 0o700);
    const logPath = path.join(COMMANDS_LOG_DIR, "commands.log");
    fs.appendFileSync(logPath, JSON.stringify(entry) + "\n", {
      mode: 0o600,
      encoding: "utf-8",
    });
    fs.chmodSync(logPath, 0o600);
  } catch {
    // Best-effort; never block the tool_call handler
  }
}

// Load global config from unified policy.json at ~/.pi/agent/custom/execution-policy/policy.json
const globalConfigRaw = JSON.parse(
  fs.readFileSync(path.join(EXT_DIR, "policy.json"), "utf-8"),
);

const globalUnified = normalizeUnifiedPolicyConfig(globalConfigRaw);

// --- Auto mode config ---

interface AutoModeConfig {
  enable: boolean;
  provider: string;
  model: string;
  thinkingEnabled?: boolean;
  timeoutMs?: number;
  maxTokens?: number;
}

interface ExtensionConfig {
  shellPolicy?: {
    autoMode?: {
      enable?: unknown;
      provider?: unknown;
      model?: unknown;
      thinkingEnabled?: unknown;
      timeoutMs?: unknown;
      maxTokens?: unknown;
    };
  };
}

function loadAutoModeConfig(): AutoModeConfig | null {
  const customPath = path.join(EXT_DIR, "config.json");
  if (!fs.existsSync(customPath)) return null;
  try {
    const raw: ExtensionConfig = JSON.parse(
      fs.readFileSync(customPath, "utf-8"),
    );
    const cfg = raw.shellPolicy?.autoMode;
    if (
      !cfg ||
      cfg.enable !== true ||
      typeof cfg.provider !== "string" ||
      typeof cfg.model !== "string"
    ) {
      return null;
    }
    return {
      enable: true,
      provider: cfg.provider,
      model: cfg.model,
      thinkingEnabled: cfg.thinkingEnabled === true ? true : undefined,
      timeoutMs: typeof cfg.timeoutMs === "number" ? cfg.timeoutMs : undefined,
      maxTokens: typeof cfg.maxTokens === "number" ? cfg.maxTokens : undefined,
    };
  } catch {
    return null;
  }
}

function loadAutoModePrompt(): string | null {
  const promptPath = path.join(EXT_DIR, "auto-mode", "prompt.md");
  try {
    return fs.readFileSync(promptPath, "utf-8");
  } catch {
    return null;
  }
}
const contextTemplateCache = new Map<string, string>();
function loadContextTemplate(mode: string): string {
  const relativePath = resolveModeContextPath(mode);
  if (relativePath === null) {
    return "";
  }
  const cached = contextTemplateCache.get(relativePath);
  if (cached !== undefined) {
    return cached;
  }

  const promptPath = path.join(EXT_DIR, relativePath);
  let template: string;
  try {
    template = fs.readFileSync(promptPath, "utf-8");
  } catch {
    template = "";
  }
  contextTemplateCache.set(relativePath, template);
  return template;
}

const autoModeConfig = loadAutoModeConfig();
const autoModePromptTemplate = autoModeConfig ? loadAutoModePrompt() : null;

const NO_COMMANDS_CONTEXT =
  "No commands context available. Evaluate the command against the general evaluation criteria only.";
const commandsContextText: string = (() => {
  try {
    const text = fs
      .readFileSync(
        path.join(EXT_DIR, "auto-mode", "commands-context.md"),
        "utf-8",
      )
      .trim();
    return text || NO_COMMANDS_CONTEXT;
  } catch {
    return NO_COMMANDS_CONTEXT;
  }
})();

interface AutoModeDecision {
  decision: "allow" | "ask";
  reason: string;
}

function extractJson(text: string): string | null {
  const fenced = text.match(/```(?:json)?\s*([\s\S]*?)```/i);
  const body = fenced ? fenced[1] : text;
  const start = body.indexOf("{");
  const end = body.lastIndexOf("}");
  if (start === -1 || end === -1 || end <= start) return null;
  return body.slice(start, end + 1);
}

function parseAutoModeDecision(text: string): AutoModeDecision | null {
  const json = extractJson(text);
  if (!json) return null;
  try {
    const parsed = JSON.parse(json) as Partial<AutoModeDecision>;
    if (parsed.decision !== "allow" && parsed.decision !== "ask") return null;
    return {
      decision: parsed.decision,
      reason: typeof parsed.reason === "string" ? parsed.reason : "",
    };
  } catch {
    return null;
  }
}

async function evaluateAutoMode(
  command: string,
  ctx: ExtensionContext,
): Promise<AutoModeDecision | null> {
  if (!autoModeConfig || !autoModePromptTemplate) return null;

  const model = ctx.modelRegistry.find(
    autoModeConfig.provider,
    autoModeConfig.model,
  );
  if (!model) {
    ctx.ui.notify(
      `Auto mode model "${autoModeConfig.provider}/${autoModeConfig.model}" not found, falling back to confirmation`,
      "warning",
    );
    return null;
  }

  const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
  if (!auth.ok || !auth.apiKey) {
    ctx.ui.notify(
      `Auto mode auth failed for ${autoModeConfig.provider}, falling back to confirmation`,
      "warning",
    );
    return null;
  }

  if (!command || !command.trim()) {
    // Empty command — no point in LLM evaluation
    return null;
  }

  const contextHint = buildContextHint(ctx);
  const promptText = autoModePromptTemplate
    .replaceAll("{CONTEXT_HINT}", contextHint)
    .replaceAll("{COMMAND}", command)
    .replaceAll("{CWD}", ctx.cwd || "(unknown)")
    .replaceAll("{COMMANDS_CONTEXT}", commandsContextText);

  // Cap the evaluation so a stalled model doesn't block the tool call.
  const signals: AbortSignal[] = [
    AbortSignal.timeout(autoModeConfig.timeoutMs ?? 30_000),
  ];
  if (ctx.signal) signals.push(ctx.signal);
  const signal = AbortSignal.any(signals);

  try {
    const response = await complete(
      model,
      {
        messages: [
          {
            role: "user",
            content: [{ type: "text", text: promptText }],
            timestamp: Date.now(),
          },
        ],
      },
      {
        apiKey: auth.apiKey,
        headers: auth.headers,
        maxTokens: autoModeConfig.maxTokens ?? model.maxTokens,
        temperature: 0,
        thinkingEnabled: autoModeConfig.thinkingEnabled === true,
        signal,
      },
    );

    const text = response.content
      .filter((c): c is { type: "text"; text: string } => c.type === "text")
      .map((c) => c.text)
      .join("\n")
      .trim();

    return parseAutoModeDecision(text);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    ctx.ui.notify(
      `Auto mode evaluation failed: ${message}, falling back to confirmation`,
      "warning",
    );
    return null;
  }
}

// Project-local config, keyed by cwd for safe multi-project support
interface ProjectPolicyCache {
  commands: PolicyCommands;
  wrappers: WrapperRuleConfig[];
}

const projectPolicyCache = new Map<string, ProjectPolicyCache>();

function getProjectPolicy(cwd: string): ProjectPolicyCache {
  const cached = projectPolicyCache.get(cwd);
  if (cached) return cached;

  const policyPath = path.join(cwd, ".pi", "policy.json");
  let commands: PolicyCommands = { allow: [], ask: [], deny: [] };
  let wrappers: WrapperRuleConfig[] = [];
  if (fs.existsSync(policyPath)) {
    try {
      const raw = JSON.parse(fs.readFileSync(policyPath, "utf-8"));
      const parsed =
        "default" in raw
          ? normalizeUnifiedPolicyConfig(raw).default
          : normalizeShellPolicyConfig(raw);
      commands = parsed.commands;
      wrappers = parsed.wrappers ?? [];
    } catch {
      // fall through to empty policy
    }
  }

  const result = { commands, wrappers };
  projectPolicyCache.set(cwd, result);
  return result;
}

async function confirmCommand(
  command: string,
  ctx: ExtensionContext,
  result: EvalResult,
): Promise<{ block: boolean; reason?: string }> {
  let autoModeReason: string | undefined;
  if (autoModeConfig) {
    const decision = await evaluateAutoMode(command, ctx);
    if (decision?.decision === "allow") {
      ctx.ui.notify(
        `Auto-approved${decision.reason ? `: ${decision.reason}` : ""}`,
        "info",
      );
      return { block: false };
    }
    autoModeReason = decision?.reason;
  }

  if (!ctx.hasUI) {
    return {
      block: true,
      reason: `Command blocked (no UI available)${formatTriggerReason(result)}`,
    };
  }

  const trigger = formatTriggerReason(result);
  const suffix = autoModeReason ? ` -- ${autoModeReason}` : "";
  const choice = await ctx.ui.select(
    `Confirm${trigger}${suffix}:\n${getCommandSummary(command)}`,
    ["Yes, proceed", "No, cancel"],
  );

  if (choice !== "Yes, proceed") {
    ctx.ui.notify("Command cancelled by user", "info");
    return { block: true, reason: "Blocked by user" };
  }

  return { block: false };
}

function formatTriggerReason(result: EvalResult): string {
  switch (result.decidedBy) {
    case "commands":
      return result.match
        ? ` (${result.match.category}: ${result.match.entry.match})`
        : " (command policy)";
    case "redirects":
      return " (redirect policy)";
    case "heredocs":
      return " (heredoc policy)";
    default:
      return result.match ? ` (${result.match.category})` : "";
  }
}

function buildContextHint(ctx: ExtensionContext): string {
  // Group modes by prefix (before first ":"), pick most specific per prefix
  const groups = new Map<string, string>();
  for (const mode of getExecutionMode(ctx).modes) {
    const prefix = mode.includes(":") ? mode.split(":")[0] : mode;
    const existing = groups.get(prefix);
    if (!existing || mode.length > existing.length) {
      groups.set(prefix, mode);
    }
  }

  // Load context files for the selected modes, in insertion order
  const parts: string[] = [];
  for (const [, mode] of groups) {
    const template = loadContextTemplate(mode);
    if (template.trim()) {
      parts.push(template.replaceAll("{CWD}", ctx.cwd ?? "(unknown)"));
    }
  }

  if (parts.length > 0) {
    return parts.join("\n\n---\n\n");
  }
  return "No execution mode context hint. Evaluate the command against the general evaluation criteria only.";
}

function formatPolicyMatch(match: EvalResult["match"]): string {
  if (!match) return "policy";
  return `${match.category}: ${match.entry.match}`;
}

export default function (pi: ExtensionAPI) {
  function isPathAllowed(
    toolName: string,
    targetPath: string | undefined,
    policyOverride: { write?: string[]; edit?: string[] } | undefined,
  ): boolean {
    if (!targetPath || !policyOverride) return false;
    const allowedPaths =
      toolName === "write" ? policyOverride.write : policyOverride.edit;
    if (!allowedPaths) return false;
    const resolvedTarget = path.resolve(targetPath);
    return allowedPaths.some(
      (allowedPath) => path.resolve(allowedPath) === resolvedTarget,
    );
  }

function mergeToolAllowedStrict(
  toolName: string,
  policies: ModePolicy[],
): boolean | undefined {
  // Most restrictive wins: any false → false, any true (no false) → true, else undefined
  let hasTrue = false;
  for (const policy of policies) {
    const value = policy.tools?.[toolName];
    if (value === false) return false;
    if (value === true) hasTrue = true;
  }
  return hasTrue ? true : undefined;
}

  function formatBlockReason(
    toolName: string,
    policyOverride: { write?: string[]; edit?: string[] } | undefined,
    mode: string,
  ): string {
    const allowedPaths =
      toolName === "write" ? policyOverride?.write : policyOverride?.edit;
    if (!allowedPaths || allowedPaths.length === 0) {
      return `Tool "${toolName}" is blocked (execution mode: ${mode}).`;
    }
    const resolved = allowedPaths.map((p) => path.resolve(p));
    return [
      `Tool "${toolName}" is blocked (execution mode: ${mode}).`,
      ...resolved.map((p) => `Allowed path: ${p}`),
    ].join("\n");
  }

  pi.on("tool_call", async (event, ctx) => {
    const executionMode = getExecutionMode(ctx);
    const { policyOverride } = executionMode;
    const modePolicies = executionMode.modes
      .map((mode) => globalUnified.modes?.[mode])
      .filter((policy): policy is ModePolicy => Boolean(policy));

    // Defense-in-depth: block any tool explicitly disabled by mode policy
    const toolAllowed = mergeToolAllowedStrict(event.toolName, modePolicies);
    if (toolAllowed === false) {
      // For write/edit, check path-specific allowances
      if (event.toolName === "write" || event.toolName === "edit") {
        const targetPath = event.input?.path as string | undefined;
        if (isPathAllowed(event.toolName, targetPath, policyOverride)) {
          return undefined; // Allow this specific path
        }
      }
      return {
        block: true,
        reason: `Tool "${event.toolName}" is blocked (execution mode: ${executionMode.mode}).`,
      };
    }

    // Write/edit now handled above by general tool policy enforcement
    if (event.toolName === "write" || event.toolName === "edit") {
      return undefined; // No additional blocking beyond tool policy
    }

    // --- Bash command evaluation ---
    if (event.toolName !== "bash") return undefined;
    if (typeof event.input?.command !== "string") return undefined;

    const command = event.input.command;
    const projectPolicy = getProjectPolicy(ctx.cwd);

    // Merge: default + mode stack + project
    const basePolicy = mergeEvaluationPolicyStackStrict(
      globalUnified.default,
      modePolicies,
    );
    const mergedPolicy: EvaluationPolicy = {
      commands: mergePoliciesStrict(
        basePolicy.commands!,
        projectPolicy.commands,
      ),
      redirects: basePolicy.redirects,
      heredocs: basePolicy.heredocs,
      wrappers: [...(basePolicy.wrappers ?? []), ...projectPolicy.wrappers],
    };

    const result = evaluate(command, mergedPolicy);

    // Log commands that require confirmation for later policy review
    if (result.action === "ask" || result.action === "default") {
      logConfirmNeeded(command, result);
    }

    switch (result.action) {
      case "deny":
        return {
          block: true,
          reason: `Command blocked by safety policy (${formatPolicyMatch(result.match)}): "${getCommandSummary(command)}"`,
        };
      case "ask":
        return await confirmCommand(command, ctx, result);
      case "allow":
        return undefined;
      case "default":
        return await confirmCommand(command, ctx, result);
    }
  });

  setModeChangeHook((_ctx, _mode, modes) => {
    const modePolicies = modes
      .map((mode) => globalUnified.modes?.[mode])
      .filter((policy): policy is ModePolicy => Boolean(policy));
    const disabledTools = computeDisabledTools(modePolicies);
    const activeToolNames = pi.getActiveTools().filter((name) => !disabledTools.has(name));
    pi.setActiveTools(activeToolNames);

  });
}
