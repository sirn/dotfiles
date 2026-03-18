#!/usr/bin/env tsx
/**
 * Shell Policy Evaluation Script
 *
 * Evaluates shell commands against the Pi shell policy to debug and understand
 * how commands are classified (allow/ask/deny) and which rules they match.
 *
 * Usage:
 *   evaluate-shell-policy.ts [options] <command...>
 *
 * Examples:
 *   evaluate-shell-policy.ts curl https://example.com
 *   evaluate-shell-policy.ts -f ./plan-mode.json ls -la
 *   evaluate-shell-policy.ts -- rm -rf /tmp/test
 */

import * as fs from "node:fs";
import * as path from "node:path";
import {
  analyze,
  compareActions,
  normalizeShellPolicyConfig,
  normalizeUnifiedPolicyConfig,
  type Token,
  type PolicyCommands,
  type WrapperRuleConfig,
  type RedirectPolicy,
  type HeredocPolicy,
  type Action,
  type CommandEvaluation,
  type ShellPolicyAnalysis,
} from "../lib/shell-policy.js";

// --- ANSI Colors ---
const B = "\x1b[1m";
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
  policyPaths: string[]; // One or more policy files to evaluate
  commandParts: string[]; // Command as individual args (to be joined)
}

// --- Helpers ---

function printUsage(): void {
  const scriptName = process.argv[1];
  console.log(`Usage: ${scriptName} [options] <command...>`);
  console.log();
  console.log("Options:");
  console.log(
    "  -f, --file <path>  Use custom policy file (can be used multiple times)",
  );
  console.log(
    "  --                 End option parsing (useful if command starts with -)",
  );
  console.log();
  console.log("Default behavior:");
  console.log("  Evaluates against policy.json (default and plan modes)");
  console.log();
  console.log("Examples:");
  console.log(`  ${scriptName} curl https://example.com`);
  console.log(`  ${scriptName} -f ./custom.json ls -la`);
  console.log(`  ${scriptName} -- rm -rf /tmp/test`);
}

const DEFAULT_POLICIES = ["~/.pi/agent/policy.json"];

function parseArgs(): CliArgs | null {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    return null; // Show usage
  }

  const policyPaths: string[] = [];
  const commandParts: string[] = [];
  let i = 0;

  // Parse options
  while (i < args.length) {
    if (args[i] === "-f" || args[i] === "--file") {
      if (i + 1 >= args.length) {
        console.error("Error: -f requires a file path");
        return null;
      }
      policyPaths.push(args[i + 1]);
      i += 2;
    } else if (args[i] === "--") {
      // Everything after -- is the command
      commandParts.push(...args.slice(i + 1));
      break;
    } else {
      // First non-option arg starts the command
      commandParts.push(...args.slice(i));
      break;
    }
  }

  // Use defaults if no -f specified
  if (policyPaths.length === 0) {
    policyPaths.push(...DEFAULT_POLICIES);
  }

  // Expand tilde in paths
  const expandedPaths = policyPaths.map((p) =>
    p.startsWith("~/") ? path.join(process.env.HOME || "", p.slice(2)) : p,
  );

  if (commandParts.length === 0) {
    return null; // No command provided
  }

  return {
    policyPaths: expandedPaths,
    commandParts,
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

function formatSource(source: CommandEvaluation["source"]): string {
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

// A single named policy entry (one per mode in unified format, or one for legacy)
interface PolicyEntry {
  name: string;
  commands: PolicyCommands;
  wrappers?: WrapperRuleConfig[];
  redirects?: RedirectPolicy;
  heredocs?: HeredocPolicy;
}

// Load policy from JSON file; returns one entry per mode for unified format
function loadPolicyEntries(jsonPath: string): PolicyEntry[] {
  const fullPath = path.resolve(jsonPath);
  if (!fs.existsSync(fullPath)) {
    throw new Error(`Policy file not found: ${fullPath}`);
  }

  const content = fs.readFileSync(fullPath, "utf-8");
  const raw = JSON.parse(content);

  if ("default" in raw) {
    // Unified format — expand into per-mode entries.
    // Modes come first to match runtime evaluation order (plan-mode runs before safety-gate).
    const unified = normalizeUnifiedPolicyConfig(raw);
    const entries: PolicyEntry[] = [];
    if (unified.modes) {
      for (const [modeName, mode] of Object.entries(unified.modes)) {
        entries.push({
          name: modeName,
          commands: mode.commands,
          wrappers: mode.wrappers,
          redirects: mode.redirects,
          heredocs: mode.heredocs,
        });
      }
    }
    entries.push({
      name: "default",
      commands: unified.default.commands,
      wrappers: unified.default.wrappers,
      redirects: unified.default.redirects,
      heredocs: unified.default.heredocs,
    });
    return entries;
  }

  // Legacy format
  const normalized = normalizeShellPolicyConfig(
    raw.commands ? raw : { commands: raw },
  );
  return [
    {
      name: path.basename(jsonPath, ".json"),
      commands: normalized.commands,
      wrappers: normalized.wrappers,
    },
  ];
}

// --- Multi-Policy Evaluation Types and Functions ---

interface PolicyResult {
  policyPath: string;
  entryName: string;
  analysis?: ShellPolicyAnalysis;
  error?: string;
}

function evaluatePolicy(policyPath: string, command: string): PolicyResult[] {
  let entries: PolicyEntry[];
  try {
    entries = loadPolicyEntries(policyPath);
  } catch (e) {
    return [
      {
        policyPath,
        entryName: path.basename(policyPath),
        error: String(e),
      },
    ];
  }

  return entries.map((entry) => ({
    policyPath,
    entryName: entry.name,
    analysis: analyze(command, {
      commands: entry.commands,
      redirects: entry.redirects,
      heredocs: entry.heredocs,
      wrappers: entry.wrappers,
    }),
  }));
}

// --- Main ---

function printResults(results: PolicyResult[], showTokens: boolean): void {
  const firstAnalysis = results.find((result) => result.analysis)?.analysis;
  if (showTokens && firstAnalysis && firstAnalysis.tokens.length > 0) {
    console.log(`${B}Tokens:${R}`);
    for (let i = 0; i < firstAnalysis.tokens.length; i++) {
      console.log(
        `  ${GRY}${i + 1}.${R} ${YEL}${formatTokenValue(firstAnalysis.tokens[i])}${R}`,
      );
    }
    console.log();
  }

  for (const result of results) {
    const displayName = `${path.basename(result.policyPath)} (${result.entryName})`;

    if (result.error || !result.analysis) {
      console.log(`${B}${displayName}:${R} ${RED}Error: ${result.error}${R}`);
      continue;
    }

    const { analysis } = result;
    console.log(`${B}${displayName}:${R}`);

    if (analysis.commands.length === 0) {
      console.log("  (no commands)");
    } else {
      for (let i = 0; i < analysis.commands.length; i++) {
        const cmd = analysis.commands[i];
        const extra =
          cmd.redirects.length > 0
            ? ` [${cmd.redirects.map((r) => `${r.op} ${r.target}`).join(", ")}]`
            : "";
        process.stdout.write(
          `  ${GRY}${i + 1}.${R} ${YEL}${cmd.fullText}${R} ${formatSource(cmd.source)}${extra}\n`,
        );
        process.stdout.write(
          `     ${B}Policy:${R} ${formatAction(cmd.action)}\n`,
        );

        if (cmd.matches.length > 0) {
          for (let j = 0; j < cmd.matches.length; j++) {
            const match = cmd.matches[j];
            const isLast = j === cmd.matches.length - 1;
            const branch = isLast ? "└─" : "├─";
            const line = isLast ? "  " : "│ ";

            process.stdout.write(
              `     ${branch} ${B}match:${R} ${YEL}${match.entry.match}${R}\n`,
            );
            process.stdout.write(
              `     ${line} ${B}mode:${R} ${match.entry.mode}\n`,
            );
            process.stdout.write(
              `     ${line} ${B}policy:${R} ${formatAction(match.category)}\n`,
            );
          }
        }
      }
    }

    if (analysis.phases.redirects) {
      console.log(
        `  ${B}Redirects:${R} ${formatAction(analysis.phases.redirects.action)} ${GRY}(${analysis.phases.redirects.reason})${R}`,
      );
    }
    if (analysis.phases.heredocs) {
      console.log(
        `  ${B}Heredocs:${R} ${formatAction(analysis.phases.heredocs.action)} ${GRY}(${analysis.phases.heredocs.reason})${R}`,
      );
    }

    console.log(
      `  ${B}Result:${R} ${formatAction(analysis.final.action)} ${GRY}(decided by: ${analysis.final.decidedBy})${R}`,
    );
    console.log();
  }

  let overall: Action = "default";
  for (const result of results) {
    if (
      result.analysis &&
      compareActions(result.analysis.final.action, overall) > 0
    ) {
      overall = result.analysis.final.action;
    }
  }

  console.log(`${B}Overall:${R} ${formatAction(overall)}`);
}

function main(): void {
  const args = parseArgs();
  if (!args) {
    printUsage();
    process.exit(1);
  }

  // Join command parts into full command string
  const command = args.commandParts.join(" ");

  // Evaluate against all specified policies (each may expand into multiple results)
  const results: PolicyResult[] = [];
  for (const policyPath of args.policyPaths) {
    const policyResults = evaluatePolicy(policyPath, command);
    results.push(...policyResults);
  }

  // Check for errors
  const errors = results.filter((r) => r.error);
  if (errors.length > 0) {
    for (const err of errors) {
      console.error(`${RED}Error loading ${err.policyPath}: ${err.error}${R}`);
    }
    if (errors.length === results.length) {
      process.exit(1);
    }
  }

  // Print results
  printResults(results, true);
}

main();
