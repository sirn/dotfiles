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
  mergeEvaluationPolicies,
  normalizeUnifiedPolicyConfig,
  type Token,
  type WrapperRuleConfig,
  type RedirectPolicy,
  type HeredocPolicy,
  type Action,
  type CommandEvaluation,
  type ShellPolicyAnalysis,
  type EvaluationPolicy,
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
  mode: string; // Always present, defaults to "edit"
}

// --- Helpers ---

function printUsage(): void {
  const scriptName = process.argv[1];
  console.log(`Usage: ${scriptName} [options] <command...>`);
  console.log();
  console.log("Options:");
  console.log(
    "  -f, --file <path>  Use custom policy file (default: ~/.pi/agent/custom/shell-policy/policy.json)",
  );
  console.log(
    "  -m, --mode <mode>  Evaluate with mode merged on top of default (default: edit)",
  );
  console.log(
    "  --                 End option parsing (useful if command starts with -)",
  );
  console.log();
  console.log("Description:");
  console.log(
    "  Evaluates shell commands against merged policy (default + mode).",
  );
  console.log(
    "  This matches the runtime behavior of the shell-policy extension.",
  );
  console.log();
  console.log("Examples:");
  console.log(
    `  ${scriptName} ls -la                    # Uses 'edit' mode (default only)`,
  );
  console.log(
    `  ${scriptName} -m plan -- ls -la         # Merges plan mode on default`,
  );
  console.log(`  ${scriptName} -f ./policy.json -- rm -rf /tmp/test`);
}

const DEFAULT_POLICIES = ["~/.pi/agent/custom/shell-policy/policy.json"];

function parseArgs(): CliArgs | null {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    return null; // Show usage
  }

  const policyPaths: string[] = [];
  const commandParts: string[] = [];
  let mode = "edit"; // Default mode
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
    } else if (args[i] === "-m" || args[i] === "--mode") {
      if (i + 1 >= args.length) {
        console.error("Error: -m requires a mode name");
        return null;
      }
      mode = args[i + 1];
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
    mode, // Always "edit" or user-specified
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

// --- Merged Policy Loading ---

// Load and merge policy for a specific mode
interface MergedPolicyResult {
  policyPath: string;
  mode: string;
  mergedPolicy: EvaluationPolicy;
  error?: string;
}

function loadMergedPolicy(jsonPath: string, mode: string): MergedPolicyResult {
  const fullPath = path.resolve(jsonPath);
  if (!fs.existsSync(fullPath)) {
    return {
      policyPath: fullPath,
      mode,
      mergedPolicy: { commands: { allow: [], ask: [], deny: [] } },
      error: `Policy file not found: ${fullPath}`,
    };
  }

  try {
    const content = fs.readFileSync(fullPath, "utf-8");
    const raw = JSON.parse(content);
    const unified = normalizeUnifiedPolicyConfig(raw);

    // Get mode policy (undefined if mode is "edit" or doesn't exist)
    const modePolicy =
      mode !== "edit" && mode !== "default" ? unified.modes?.[mode] : undefined;

    if (mode !== "edit" && mode !== "default" && !modePolicy) {
      const availableModes = unified.modes
        ? Object.keys(unified.modes).join(", ")
        : "none";
      return {
        policyPath: fullPath,
        mode,
        mergedPolicy: { commands: unified.default.commands },
        error: `Mode "${mode}" not found in ${path.basename(fullPath)}. Available modes: ${availableModes}`,
      };
    }

    // Merge: default + mode (same as shell-policy.ts)
    const merged = mergeEvaluationPolicies(unified.default, modePolicy);

    return {
      policyPath: fullPath,
      mode,
      mergedPolicy: merged,
    };
  } catch (e) {
    return {
      policyPath: fullPath,
      mode,
      mergedPolicy: { commands: { allow: [], ask: [], deny: [] } },
      error: String(e),
    };
  }
}

// --- Multi-Policy Evaluation Types and Functions ---

interface PolicyResult {
  policyPath: string;
  mode: string; // Track which mode was used
  analysis?: ShellPolicyAnalysis;
  error?: string;
}

function evaluateMergedPolicy(
  policyPath: string,
  command: string,
  mode: string,
): PolicyResult {
  const loaded = loadMergedPolicy(policyPath, mode);

  if (loaded.error) {
    return {
      policyPath,
      mode,
      error: loaded.error,
    };
  }

  const analysis = analyze(command, loaded.mergedPolicy);

  return {
    policyPath,
    mode,
    analysis,
  };
}

// --- Main ---

function printResults(results: PolicyResult[], showTokens: boolean): void {
  const firstResult = results[0];
  if (!firstResult) {
    console.log("No results to display.");
    return;
  }

  // Show mode header
  const modeDisplay =
    firstResult.mode === "edit"
      ? "edit (default only, no mode-specific rules)"
      : `${firstResult.mode} (merged with default)`;
  console.log(`${B}Mode:${R} ${modeDisplay}`);
  console.log();

  const firstAnalysis = results.find((r) => r.analysis)?.analysis;
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
    const displayName = `${path.basename(result.policyPath)} (merged)`;

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

  // Calculate overall from merged results
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

  // Evaluate against merged policy for each policy file
  const results: PolicyResult[] = [];
  for (const policyPath of args.policyPaths) {
    const result = evaluateMergedPolicy(policyPath, command, args.mode);
    results.push(result);
  }

  // Check for errors
  const errors = results.filter((r) => r.error);
  if (errors.length > 0) {
    for (const err of errors) {
      console.error(`${RED}Error: ${err.error}${R}`);
    }
    if (errors.length === results.length) {
      process.exit(1);
    }
  }

  // Print results
  printResults(results, true);
}

main();
