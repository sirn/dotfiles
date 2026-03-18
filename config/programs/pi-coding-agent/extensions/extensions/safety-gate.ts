/**
 * Safety Gate Extension for Pi Coding Agent
 *
 * Loads command policies from safety-gate.json and enforces them via tool_call hooks.
 * - allow: commands permitted without confirmation
 * - ask: commands requiring user confirmation
 * - deny: commands that are blocked entirely
 *
 * Policy: Ask by default - any command not explicitly allowed or denied requires confirmation.
 * Per-project overrides can be placed in .pi/policy.json relative to the project root.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { readFileSync, existsSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import {
  evaluate,
  mergePolicies,
  normalizeUnifiedPolicyConfig,
  normalizeShellPolicyConfig,
  getCommandSummary,
  type EvalResult,
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
const globalConfig: PolicyCommands = globalUnified.default.commands;
const globalWrapperRules: WrapperRuleConfig[] =
  globalUnified.default.wrappers ?? [];
const globalHeredocPolicy: HeredocPolicy = globalUnified.default.heredocs ?? {
  action: "ask",
};

// Project-local config, loaded lazily on first tool_call (cwd is static per session)
let projectConfig: PolicyCommands | null = null;
let projectWrapperRules: WrapperRuleConfig[] = [];

function getProjectConfig(cwd: string): PolicyCommands {
  if (projectConfig !== null) return projectConfig;
  const policyPath = join(cwd, ".pi", "policy.json");
  if (existsSync(policyPath)) {
    try {
      const raw = JSON.parse(readFileSync(policyPath, "utf-8"));
      const parsed =
        "default" in raw
          ? normalizeUnifiedPolicyConfig(raw).default
          : normalizeShellPolicyConfig(raw);
      projectConfig = parsed.commands;
      projectWrapperRules = parsed.wrappers ?? [];
      return projectConfig;
    } catch {
      // fall through to empty policy
    }
  }
  projectConfig = { allow: [], ask: [], deny: [] };
  projectWrapperRules = [];
  return projectConfig;
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
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return undefined;

    if (typeof event.input?.command !== "string") return undefined;
    const command = event.input.command;
    const projectCommands = getProjectConfig(ctx.cwd);

    const result = evaluate(command, {
      commands: mergePolicies(globalConfig, projectCommands),
      heredocs: globalHeredocPolicy,
      wrappers: [...globalWrapperRules, ...projectWrapperRules],
    });

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
