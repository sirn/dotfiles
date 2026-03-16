/**
 * Safety Gate Extension for Pi Coding Agent
 *
 * Loads command policies from safety-gate.json and enforces them via tool_call hooks.
 * - allow: commands permitted without confirmation
 * - ask: commands requiring user confirmation
 * - deny: commands that are blocked entirely
 *
 * Policy: Ask by default - any command not explicitly allowed or denied requires confirmation.
 * Per-project overrides can be placed in .pi/safety-gate.json relative to the project root.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { readFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import {
  evaluateCommand,
  extractCommands,
  tokenize,
  buildWrapperRuleMap,
  normalizeShellPolicyConfig,
  getCommandSummary,
  type PolicyCommands,
  type WrapperRuleConfig,
  type ExtractedCommand,
} from "../lib/shell-policy.js";

function hasHeredoc(cmd: ExtractedCommand): boolean {
  return cmd.redirects.some((r) => r.op === "<<" || r.op === "<<-");
}

// Load global config from JSON file in the same directory
const __dirname = dirname(fileURLToPath(import.meta.url));
const globalConfigRaw = JSON.parse(
  readFileSync(join(__dirname, "../safety-gate.json"), "utf-8")
);

const globalParsed = normalizeShellPolicyConfig(globalConfigRaw);
const globalConfig: PolicyCommands = globalParsed.commands;
let globalWrapperRules: WrapperRuleConfig[] = globalParsed.wrappers ?? [];

// Project-local config, loaded lazily on first tool_call (cwd is static per session)
let projectConfig: PolicyCommands | null = null;
let projectWrapperRules: WrapperRuleConfig[] = [];

function getProjectConfig(cwd: string): PolicyCommands {
  if (projectConfig !== null) return projectConfig;
  try {
    const parsed = normalizeShellPolicyConfig(
      JSON.parse(readFileSync(join(cwd, ".pi", "safety-gate.json"), "utf-8"))
    );
    projectConfig = parsed.commands;
    projectWrapperRules = parsed.wrappers ?? [];
  } catch {
    projectConfig = { allow: [], ask: [], deny: [] };
    projectWrapperRules = [];
  }
  return projectConfig;
}

async function confirmCommand(
  command: string,
  ctx: ExtensionAPI["context"]
): Promise<{ block: boolean; reason?: string }> {
  if (!ctx.hasUI) {
    return {
      block: true,
      reason: `Command blocked (no UI for confirmation): "${getCommandSummary(command)}"`,
    };
  }

  const choice = await ctx.ui.select(
    `Confirm: ${getCommandSummary(command)}`,
    ["Yes, proceed", "No, cancel"]
  );

  if (choice !== "Yes, proceed") {
    ctx.ui.notify("Command cancelled by user", "info");
    return { block: true, reason: "Blocked by user" };
  }

  return { block: false };
}

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return undefined;

    if (typeof event.input?.command !== "string") return undefined;
    const command = event.input.command;
    const merged = { allow: globalConfig.allow.concat(getProjectConfig(ctx.cwd).allow), ask: globalConfig.ask.concat(getProjectConfig(ctx.cwd).ask), deny: globalConfig.deny.concat(getProjectConfig(ctx.cwd).deny) };
    const wrapperRules = buildWrapperRuleMap([
      ...globalWrapperRules,
      ...projectWrapperRules,
    ]);

    // First evaluate against the merged JSON policy
    const result = evaluateCommand(command, merged, wrapperRules);

    // Check local structural rules (heredocs) on extracted commands
    const extractedCmds = extractCommands(tokenize(command), "direct", wrapperRules);

    for (const extractedCmd of extractedCmds) {
      // Ask for heredocs
      if (hasHeredoc(extractedCmd)) {
        return await confirmCommand(command, ctx);
      }
    }

    switch (result.action) {
      case "deny":
        return {
          block: true,
          reason: `Command blocked by safety policy: "${getCommandSummary(command)}"`,
        };
      case "ask":
        return await confirmCommand(command, ctx);
      case "allow":
        return undefined;
      case "default":
        return await confirmCommand(command, ctx);
    }
  });
}
