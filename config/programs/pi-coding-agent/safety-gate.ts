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

interface SafetyConfig {
  allow: string[];
  ask: string[];
  deny: string[];
}

interface Patterns {
  allow: RegExp[];
  ask: RegExp[];
  deny: RegExp[];
}

// Load global config from JSON file in the same directory
const __dirname = dirname(fileURLToPath(import.meta.url));
const globalConfig: SafetyConfig = JSON.parse(
  readFileSync(join(__dirname, "safety-gate.json"), "utf-8")
);

// Convert command strings to regex patterns
function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function toRegex(cmd: string): RegExp {
  // Raw regex pattern: re:<pattern>
  if (cmd.startsWith("re:")) {
    const pattern = cmd.slice(3);
    try {
      return new RegExp(pattern, "i");
    } catch (e) {
      throw new Error(`Invalid regex in safety-gate config "${cmd}": ${e}`);
    }
  }

  const escaped = escapeRegex(cmd);
  if (cmd.includes("/")) {
    // Path pattern: match anywhere (for MCP wrappers and path-prefixed commands)
    return new RegExp(`${escaped}\\b`, "i");
  }
  // Command pattern: match at start or after shell operators
  return new RegExp(
    `(?:^|\\|\\||&&|[&;|]|[({` + "`" + `\\n])\\s*${escaped}\\b`,
    "i"
  );
}

function toPatterns(cfg: SafetyConfig): Patterns {
  return {
    allow: cfg.allow.map(toRegex),
    ask: cfg.ask.map(toRegex),
    deny: cfg.deny.map(toRegex),
  };
}

const globalPatterns = toPatterns(globalConfig);

// Project-local patterns, loaded lazily on first tool_call (cwd is static per session)
let projectPatterns: Patterns | null = null;

function getProjectPatterns(cwd: string): Patterns {
  if (projectPatterns !== null) return projectPatterns;
  try {
    const local: Partial<SafetyConfig> = JSON.parse(
      readFileSync(join(cwd, ".pi", "safety-gate.json"), "utf-8")
    );
    projectPatterns = toPatterns({
      allow: local.allow ?? [],
      ask: local.ask ?? [],
      deny: local.deny ?? [],
    });
  } catch {
    projectPatterns = { allow: [], ask: [], deny: [] };
  }
  return projectPatterns;
}

// Check if a command matches any pattern
function matchesPattern(command: string, patterns: RegExp[]): boolean {
  return patterns.some((pattern) => pattern.test(command));
}

// Extract the base command for display
function getCommandSummary(command: string): string {
  if (command.length > 80) {
    return command.slice(0, 77) + "...";
  }
  return command;
}

// Prompt user for confirmation
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

    const command = event.input.command as string;
    const project = getProjectPatterns(ctx.cwd);

    // 1. Check deny patterns first (highest priority; global or project can deny)
    if (
      matchesPattern(command, globalPatterns.deny) ||
      matchesPattern(command, project.deny)
    ) {
      return {
        block: true,
        reason: `Command blocked by safety policy: "${getCommandSummary(command)}"`,
      };
    }

    // 2. Check ask patterns (require confirmation, overrides allow)
    if (
      matchesPattern(command, globalPatterns.ask) ||
      matchesPattern(command, project.ask)
    ) {
      const result = await confirmCommand(command, ctx);
      if (result.block) return result;
      return undefined; // User confirmed, proceed
    }

    // 3. Check allow patterns (proceed without confirmation)
    if (
      matchesPattern(command, project.allow) ||
      matchesPattern(command, globalPatterns.allow)
    ) {
      return undefined;
    }

    // 4. Default policy: ask for confirmation
    const result = await confirmCommand(command, ctx);
    if (result.block) return result;
    return undefined;
  });
}
