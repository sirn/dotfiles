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
 * Auto mode: when policyAutoMode is enabled in custom.json, commands that would
 * normally require user confirmation are first evaluated by a small LLM using
 * POLICY_AUTO_MODE.md. If the model returns "allow", the command runs without
 * prompting the user. Any other outcome falls back to human confirmation.
 */

import { complete } from "@mariozechner/pi-ai";
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import { getExecutionMode } from "./lib/execution-mode.js";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import {
  evaluate,
  mergePolicies,
  mergeEvaluationPolicies,
  normalizeUnifiedPolicyConfig,
  normalizeShellPolicyConfig,
  getCommandSummary,
  type EvalResult,
  type EvaluationPolicy,
  type PolicyCommands,
  type WrapperRuleConfig,
} from "./lib/shell-policy.js";

const PI_AGENT_DIR = path.join(os.homedir(), ".pi/agent");

// Load global config from unified policy.json at ~/.pi/agent/policy.json
const globalConfigRaw = JSON.parse(
  fs.readFileSync(path.join(PI_AGENT_DIR, "policy.json"), "utf-8"),
);

const globalUnified = normalizeUnifiedPolicyConfig(globalConfigRaw);

// --- Auto mode config ---

interface PolicyAutoModeConfig {
  enable: boolean;
  provider: string;
  model: string;
}

interface CustomConfig {
  policyAutoMode?: {
    enable?: unknown;
    provider?: unknown;
    model?: unknown;
  };
}

function loadAutoModeConfig(): PolicyAutoModeConfig | null {
  const customPath = path.join(PI_AGENT_DIR, "custom.json");
  if (!fs.existsSync(customPath)) return null;
  try {
    const raw: CustomConfig = JSON.parse(fs.readFileSync(customPath, "utf-8"));
    const cfg = raw.policyAutoMode;
    if (
      !cfg ||
      cfg.enable !== true ||
      typeof cfg.provider !== "string" ||
      typeof cfg.model !== "string"
    ) {
      return null;
    }
    return { enable: true, provider: cfg.provider, model: cfg.model };
  } catch {
    return null;
  }
}

function loadAutoModePrompt(): string | null {
  const promptPath = path.join(PI_AGENT_DIR, "POLICY_AUTO_MODE.md");
  try {
    return fs.readFileSync(promptPath, "utf-8");
  } catch {
    return null;
  }
}

const autoModeConfig = loadAutoModeConfig();
const autoModePromptTemplate = autoModeConfig ? loadAutoModePrompt() : null;

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

  const promptText = autoModePromptTemplate
    .replaceAll("{COMMAND}", command)
    .replaceAll("{CWD}", ctx.cwd);

  // Cap the evaluation at 10s so a stalled model doesn't block the tool call.
  const signals: AbortSignal[] = [AbortSignal.timeout(10_000)];
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
        maxTokens: 1500,
        temperature: 0,
        thinkingEnabled: false,
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
      reason: `Command blocked (no UI for confirmation): "${getCommandSummary(command)}"`,
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

  pi.on("tool_call", async (event, ctx) => {
    const { mode: currentMode, policyOverride } = getExecutionMode(ctx);
    const modePolicy = globalUnified.modes?.[currentMode];

    // --- Write/Edit tool blocking based on mode's tools config ---
    if (event.toolName === "write" || event.toolName === "edit") {
      if (modePolicy?.tools) {
        const toolAllowed = modePolicy.tools[event.toolName] ?? true;
        if (!toolAllowed) {
          // Check if this path is explicitly allowed by policy override
          const targetPath = event.input?.path as string | undefined;
          if (isPathAllowed(event.toolName, targetPath, policyOverride)) {
            return undefined; // Allow this specific path
          }
          return {
            block: true,
            reason: `Tool "${event.toolName}" blocked by "${currentMode}" mode policy`,
          };
        }
      }
      return undefined; // No opinion on write/edit otherwise
    }

    // --- Bash command evaluation ---
    if (event.toolName !== "bash") return undefined;
    if (typeof event.input?.command !== "string") return undefined;

    const command = event.input.command;
    const projectPolicy = getProjectPolicy(ctx.cwd);

    // Merge: default + mode + project
    const basePolicy = mergeEvaluationPolicies(
      globalUnified.default,
      modePolicy,
    );
    const mergedPolicy: EvaluationPolicy = {
      commands: mergePolicies(basePolicy.commands!, projectPolicy.commands),
      redirects: basePolicy.redirects,
      heredocs: basePolicy.heredocs,
      wrappers: [...(basePolicy.wrappers ?? []), ...projectPolicy.wrappers],
    };

    const result = evaluate(command, mergedPolicy);

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
}
