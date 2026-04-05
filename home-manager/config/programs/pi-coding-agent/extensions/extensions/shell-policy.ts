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
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { getExecutionMode } from "../lib/execution-mode.js";
import { readFileSync, existsSync } from "node:fs";
import path, { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
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
  type HeredocPolicy,
  type WrapperRuleConfig,
} from "../lib/shell-policy.js";

// Load global config from unified policy.json
const __dirname = dirname(fileURLToPath(import.meta.url));
const globalConfigRaw = JSON.parse(
  readFileSync(join(__dirname, "../../../policy.json"), "utf-8"),
);

const globalUnified = normalizeUnifiedPolicyConfig(globalConfigRaw);

// Project-local config, keyed by cwd for safe multi-project support
interface ProjectPolicyCache {
  commands: PolicyCommands;
  wrappers: WrapperRuleConfig[];
}

const projectPolicyCache = new Map<string, ProjectPolicyCache>();

function getProjectPolicy(cwd: string): ProjectPolicyCache {
  const cached = projectPolicyCache.get(cwd);
  if (cached) return cached;

  const policyPath = join(cwd, ".pi", "policy.json");
  let commands: PolicyCommands = { allow: [], ask: [], deny: [] };
  let wrappers: WrapperRuleConfig[] = [];
  if (existsSync(policyPath)) {
    try {
      const raw = JSON.parse(readFileSync(policyPath, "utf-8"));
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
  ctx: ExtensionAPI["context"],
  result: EvalResult,
): Promise<{ block: boolean; reason?: string }> {
  if (!ctx.hasUI) {
    return {
      block: true,
      reason: `Command blocked (no UI for confirmation): "${getCommandSummary(command)}"`,
    };
  }

  const trigger = formatTriggerReason(result);
  const choice = await ctx.ui.select(
    `Confirm${trigger}: ${getCommandSummary(command)}`,
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
