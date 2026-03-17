#!/usr/bin/env tsx
/**
 * Shell Policy Evaluation Script
 *
 * Evaluates shell commands against the Pi shell policy to debug and understand
 * how commands are classified (allow/ask/deny) and which rules they match.
 *
 * Usage:
 *   nix run nixpkgs#tsx scripts/evaluate-shell-policy.ts <permissions.json> <command>
 *
 * Example:
 *   nix run nixpkgs#tsx scripts/evaluate-shell-policy.ts ./plan-mode.json "rm -rf /"
 */

import * as fs from "node:fs";
import * as path from "node:path";
import {
  tokenize,
  extractCommands,
  evaluateCommand,
  buildWrapperRuleMap,
  normalizeShellPolicyConfig,
  type Token,
  type ExtractedCommand,
  type CommandEntry,
  type PolicyCommands,
  type WrapperRuleConfig,
  type Action,
} from "../lib/shell-policy.js";

// --- ANSI Colors ---
const B = "\x1b[1m";
const D = "\x1b[2m";
const R = "\x1b[0m";
const GRY = "\x1b[90m";
const RED = "\x1b[31m";
const GRN = "\x1b[32m";
const ORG = "\x1b[38;5;208m"; // Orange
const YEL = "\x1b[33m"; // Yellow for commands/tokens
const BLU = "\x1b[34m";
const WHT = "\x1b[37m"; // White for default

// --- Types for CLI ---

interface CliArgs {
  permissionsJsonPath: string;
  commandToTest: string;
}

// --- Helpers ---

function printUsage(): void {
  console.log(
    `Usage: nix run nixpkgs#tsx ${process.argv[1]} <permissions.json> <command>`,
  );
  console.log();
  console.log("Examples:");
  console.log(
    '  nix run nixpkgs#tsx scripts/evaluate-shell-policy.ts ./plan-mode.json "ls -la"',
  );
  console.log(
    "  nix run nixpkgs#tsx scripts/evaluate-shell-policy.ts ./plan-mode.json 'bash -c \"rm -rf /\"'",
  );
}

function parseArgs(): CliArgs | null {
  const args = process.argv.slice(2);
  if (args.length < 2) {
    return null;
  }
  return {
    permissionsJsonPath: args[0],
    commandToTest: args.slice(1).join(" "),
  };
}

function formatTokenValue(token: Token): string {
  switch (token.type) {
    case "word":
      return `"${token.value}"`;
    case "operator":
      return token.value;
    case "redirect":
      return `${token.op} ${token.target}`;
    case "group":
      return `[${token.kind}: ${token.tokens.map(formatTokenValue).join(" ")}]`;
    default:
      return JSON.stringify(token);
  }
}

function formatSource(source: ExtractedCommand["source"]): string {
  const icons: Record<typeof source, string> = {
    direct: "→",
    "wrapper-arg": "↳",
    subshell: "( )",
    substitution: "$()",
  };
  return `${GRY}${icons[source]}${R} ${BLU}${source}${R}`;
}

function formatAction(action: Action): string {
  const colors: Record<Action, string> = {
    allow: GRN,
    ask: ORG, // Orange for ask
    deny: RED,
    default: WHT, // White for default
  };
  const icons: Record<Action, string> = {
    allow: "✓",
    ask: "?",
    deny: "✗",
    default: "○",
  };
  return `${colors[action]}${icons[action]} ${action}${R}`;
}

// Load policy from JSON file
function loadPolicy(jsonPath: string): {
  commands: PolicyCommands;
  wrappers?: WrapperRuleConfig[];
} {
  const fullPath = path.resolve(jsonPath);
  if (!fs.existsSync(fullPath)) {
    throw new Error(`Policy file not found: ${fullPath}`);
  }

  const content = fs.readFileSync(fullPath, "utf-8");
  const raw = JSON.parse(content);

  const normalized = normalizeShellPolicyConfig(
    raw.commands ? raw : { commands: raw },
  );

  const wrappers = Array.isArray(raw.wrappers)
    ? (raw.wrappers as WrapperRuleConfig[])
    : undefined;

  return {
    commands: normalized.commands,
    wrappers,
  };
}

// Evaluate a single command against policy entries
function evaluateAgainstEntries(
  cmd: ExtractedCommand,
  entries: CommandEntry[],
): { matched: boolean; matches: CommandEntry[] } {
  const matches: CommandEntry[] = [];
  for (const entry of entries) {
    const { match, mode } = entry;
    const text = cmd.fullText;
    let matched = false;
    switch (mode) {
      case "exact":
        matched = text.trim().toLowerCase() === match.toLowerCase();
        break;
      case "prefix":
        matched = text
          .trimStart()
          .toLowerCase()
          .startsWith(match.toLowerCase());
        break;
      case "substring":
        matched = text.toLowerCase().includes(match.toLowerCase());
        break;
    }
    if (matched) {
      matches.push(entry);
    }
  }
  return { matched: matches.length > 0, matches };
}

// --- Main ---

function main(): void {
  const args = parseArgs();
  if (!args) {
    printUsage();
    process.exit(1);
  }

  // Load policy
  let policy: ReturnType<typeof loadPolicy>;
  try {
    policy = loadPolicy(args.permissionsJsonPath);
  } catch (e) {
    console.error(`Error: ${e}`);
    process.exit(1);
  }

  // Tokenize
  let tokens: Token[];
  try {
    tokens = tokenize(args.commandToTest);
  } catch (e) {
    console.log(`${B}Tokens:${R} (parse error)`);
    console.log(`\n${B}Policy:${R} ${formatAction("ask")}`);
    process.exit(0);
  }

  // Tokens - numbered list
  console.log(`${B}Tokens:${R}`);
  if (tokens.length === 0) {
    console.log("  (none)");
  } else {
    for (let i = 0; i < tokens.length; i++) {
      console.log(
        `  ${GRY}${i + 1}.${R} ${YEL}${formatTokenValue(tokens[i])}${R}`,
      );
    }
  }

  // Build wrapper rules
  const wrapperRules = buildWrapperRuleMap(policy.wrappers);

  // Extract commands
  let commands: ExtractedCommand[];
  try {
    commands = extractCommands(tokens, "direct", wrapperRules);
  } catch (e) {
    console.log(`\n${B}Commands:${R} (extraction error)`);
    console.log(`\n${B}Policy:${R} ${formatAction("ask")}`);
    process.exit(0);
  }

  // Commands
  console.log(`\n${B}Commands:${R}`);
  if (commands.length === 0) {
    console.log("  (none)");
  } else {
    for (let i = 0; i < commands.length; i++) {
      const cmd = commands[i];
      const extra =
        cmd.redirects.length > 0
          ? ` [${cmd.redirects.map((r) => `${r.op} ${r.target}`).join(", ")}]`
          : "";
      process.stdout.write(
        `  ${GRY}${i + 1}.${R} ${YEL}${cmd.fullText}${R} ${formatSource(cmd.source)}${extra}\n`,
      );

      // Collect all matches from all categories
      const denyResult = evaluateAgainstEntries(cmd, policy.commands.deny);
      const askResult = evaluateAgainstEntries(cmd, policy.commands.ask);
      const allowResult = evaluateAgainstEntries(cmd, policy.commands.allow);

      // Determine action and collect all matches with their categories
      type MatchInfo = { match: string; mode: string; category: Action };
      const allMatches: MatchInfo[] = [];

      if (denyResult.matched) {
        for (const m of denyResult.matches) {
          allMatches.push({
            match: m.match,
            mode: m.mode,
            category: "deny",
          });
        }
      }
      if (askResult.matched) {
        for (const m of askResult.matches) {
          allMatches.push({
            match: m.match,
            mode: m.mode,
            category: "ask",
          });
        }
      }
      if (allowResult.matched) {
        for (const m of allowResult.matches) {
          allMatches.push({
            match: m.match,
            mode: m.mode,
            category: "allow",
          });
        }
      }

      // Determine final action (deny > ask > allow > default)
      let action: Action = "default";
      if (denyResult.matched) {
        action = "deny";
      } else if (askResult.matched) {
        action = "ask";
      } else if (allowResult.matched) {
        action = "allow";
      }

      // Output Policy line
      process.stdout.write(`     ${B}Policy:${R} ${formatAction(action)}\n`);

      // Output matches in tree format
      if (allMatches.length > 0) {
        for (let j = 0; j < allMatches.length; j++) {
          const m = allMatches[j];
          const isLast = j === allMatches.length - 1;
          const branch = isLast ? "`" : "|";
          const line = isLast ? "  " : "| ";

          process.stdout.write(
            `     ${branch}- ${B}match:${R} ${YEL}${m.match}${R}\n`,
          );
          process.stdout.write(`     ${line} ${B}mode:${R} ${m.mode}\n`);
          process.stdout.write(
            `     ${line} ${B}policy:${R} ${formatAction(m.category)}\n`,
          );
        }
      }
    }
  }

  // Final verdict
  const evalResult = evaluateCommand(
    args.commandToTest,
    policy.commands,
    wrapperRules,
  );
  console.log(`\n${B}Policy:${R} ${formatAction(evalResult.action)}`);
}

main();
