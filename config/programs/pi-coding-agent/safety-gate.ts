/**
 * Safety Gate Extension for Pi Coding Agent
 *
 * Loads command policies from safety-gate.json and enforces them via tool_call hooks.
 * - allow: commands permitted without confirmation
 * - ask: commands requiring user confirmation
 * - deny: commands that are blocked entirely
 *
 * Policy: Ask by default - any command not explicitly allowed or denied requires confirmation.
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

// Load config from JSON file in the same directory
const __dirname = dirname(fileURLToPath(import.meta.url));
const configPath = join(__dirname, "safety-gate.json");
const config: SafetyConfig = JSON.parse(readFileSync(configPath, "utf-8"));

// Convert command strings to regex patterns
function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function toRegex(cmd: string): RegExp {
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

// Convert config arrays to regex patterns
const allowPatterns = config.allow.map(toRegex);
const askPatterns = config.ask.map(toRegex);
const denyPatterns = config.deny.map(toRegex);

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

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return undefined;

    const command = event.input.command as string;

    // Check deny patterns first (highest priority)
    if (matchesPattern(command, denyPatterns)) {
      return {
        block: true,
        reason: `Command blocked by safety policy: "${getCommandSummary(command)}"`,
      };
    }

    // Check allow patterns - permit without asking
    if (matchesPattern(command, allowPatterns)) {
      return undefined;
    }

    // Default policy: ask for confirmation on any command not explicitly allowed
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

    return undefined;
  });
}
