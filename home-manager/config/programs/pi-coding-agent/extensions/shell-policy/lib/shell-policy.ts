/**
 * Shell Policy Library — AST-based command evaluation for Pi extensions.
 *
 * Tokenizes shell command strings structurally and evaluates every sub-command
 * (pipes, &&/||/;, subshells, command substitution, wrapper recursion) against
 * a policy table. Fails closed on parse errors (unknown → "ask").
 */

// Shared Types

export interface CommandEntry {
  match: string;
  mode: "exact" | "prefix" | "substring" | "args";
}

export interface PolicyCommands {
  allow: CommandEntry[];
  ask: CommandEntry[];
  deny: CommandEntry[];
}

export type Action = "allow" | "ask" | "deny" | "default";

export interface PolicyMatch {
  category: "allow" | "ask" | "deny";
  entry: CommandEntry;
}

export interface WrapperRuleConfig {
  name: string;
  kind: WrapperKind;
}

export interface ShellPolicyConfig {
  commands: PolicyCommands;
  wrappers?: WrapperRuleConfig[];
}

export interface RedirectPolicy {
  action: Action;
  safeTargets?: string[];
  allowFdDup?: boolean;
}

export interface HeredocPolicy {
  action: Action;
}

export interface ModePolicy {
  tools?: Record<string, boolean>;
  commands: PolicyCommands;
  wrappers?: WrapperRuleConfig[];
  redirects?: RedirectPolicy;
  heredocs?: HeredocPolicy;
}

export interface UnifiedPolicyConfig {
  default: ModePolicy;
  modes?: Record<string, ModePolicy>;
}

function normalizePolicyCommands(raw: unknown): PolicyCommands {
  if (typeof raw !== "object" || raw === null) {
    return { allow: [], ask: [], deny: [] };
  }
  const candidate = raw as Partial<PolicyCommands>;
  return {
    allow: Array.isArray(candidate.allow) ? candidate.allow : [],
    ask: Array.isArray(candidate.ask) ? candidate.ask : [],
    deny: Array.isArray(candidate.deny) ? candidate.deny : [],
  };
}

function normalizeWrapperRuleConfig(raw: unknown): WrapperRuleConfig | null {
  if (typeof raw !== "object" || raw === null) return null;
  const candidate = raw as { name?: unknown; kind?: unknown };
  if (typeof candidate.name !== "string") return null;
  if (typeof candidate.kind !== "string") return null;
  const kind = candidate.kind as WrapperKind;
  if (
    kind !== "shell-c" &&
    kind !== "utility-operand" &&
    kind !== "env" &&
    kind !== "xargs" &&
    kind !== "docker-run"
  ) {
    return null;
  }
  return { name: candidate.name, kind };
}

export function normalizeShellPolicyConfig(raw: unknown): ShellPolicyConfig {
  if (typeof raw !== "object" || raw === null) {
    return { commands: { allow: [], ask: [], deny: [] } };
  }
  const obj = raw as Record<string, unknown>;
  if ("commands" in obj) {
    const commands = normalizePolicyCommands(obj.commands);
    const wrappers = Array.isArray(obj.wrappers)
      ? obj.wrappers
          .map(normalizeWrapperRuleConfig)
          .filter((entry): entry is WrapperRuleConfig => entry !== null)
      : undefined;
    return { commands, wrappers };
  }
  return { commands: normalizePolicyCommands(obj) };
}

function normalizeAction(raw: unknown): Action {
  if (raw === "allow" || raw === "ask" || raw === "deny" || raw === "default")
    return raw;
  return "allow";
}

export function normalizeRedirectPolicy(raw: unknown): RedirectPolicy {
  if (typeof raw !== "object" || raw === null) return { action: "allow" };
  const obj = raw as Record<string, unknown>;
  const action = normalizeAction(obj.action);
  const safeTargets = Array.isArray(obj.safeTargets)
    ? obj.safeTargets.filter((t): t is string => typeof t === "string")
    : undefined;
  const allowFdDup =
    typeof obj.allowFdDup === "boolean" ? obj.allowFdDup : undefined;
  return { action, safeTargets, allowFdDup };
}

export function normalizeHeredocPolicy(raw: unknown): HeredocPolicy {
  if (typeof raw !== "object" || raw === null) return { action: "allow" };
  const obj = raw as Record<string, unknown>;
  return { action: normalizeAction(obj.action) };
}

export function normalizeModePolicy(raw: unknown): ModePolicy {
  if (typeof raw !== "object" || raw === null) {
    return { commands: { allow: [], ask: [], deny: [] } };
  }
  const obj = raw as Record<string, unknown>;
  const commands = normalizePolicyCommands(obj.commands);
  const wrappers = Array.isArray(obj.wrappers)
    ? obj.wrappers
        .map(normalizeWrapperRuleConfig)
        .filter((w): w is WrapperRuleConfig => w !== null)
    : undefined;
  const redirects =
    obj.redirects !== undefined
      ? normalizeRedirectPolicy(obj.redirects)
      : undefined;
  const heredocs =
    obj.heredocs !== undefined
      ? normalizeHeredocPolicy(obj.heredocs)
      : undefined;
  const tools =
    typeof obj.tools === "object" && obj.tools !== null
      ? (obj.tools as Record<string, boolean>)
      : undefined;
  return { commands, wrappers, redirects, heredocs, tools };
}

export function normalizeUnifiedPolicyConfig(
  raw: unknown,
): UnifiedPolicyConfig {
  if (typeof raw !== "object" || raw === null) {
    return { default: { commands: { allow: [], ask: [], deny: [] } } };
  }
  const obj = raw as Record<string, unknown>;
  const defaultPolicy = normalizeModePolicy(obj.default);
  const modes: Record<string, ModePolicy> = {};
  if (typeof obj.modes === "object" && obj.modes !== null) {
    for (const [key, value] of Object.entries(
      obj.modes as Record<string, unknown>,
    )) {
      modes[key] = normalizeModePolicy(value);
    }
  }
  return {
    default: defaultPolicy,
    modes: Object.keys(modes).length > 0 ? modes : undefined,
  };
}

// Tool Policy Helpers

/**
 * Compute the set of tools explicitly DISABLED by a stack of mode policies.
 * If any policy in the stack disables a tool, it remains disabled (any-false-wins).
 */
export function computeDisabledTools(policies: ModePolicy[]): Set<string> {
  const disabled = new Set<string>();
  for (const policy of policies) {
    if (!policy.tools) continue;
    for (const [toolName, allowed] of Object.entries(policy.tools)) {
      if (allowed === false) {
        disabled.add(toolName);
      }
    }
  }
  return disabled;
}

/**
 * Get all tool names that have an explicit opinion in the policy stack,
 * mapped to their final opinion (true/false) after resolving the stack.
 * Last opinion wins.
 */
export function getToolsWithPolicyOpinion(
  policies: ModePolicy[],
): Map<string, boolean> {
  const result = new Map<string, boolean>();
  for (const policy of policies) {
    if (policy.tools) {
      for (const [toolName, opinion] of Object.entries(policy.tools)) {
        result.set(toolName, opinion);
      }
    }
  }
  return result;
}

// Tokenizer

interface WordToken {
  type: "word";
  value: string;
}
interface OpToken {
  type: "operator";
  value: string;
}
interface GroupToken {
  type: "group";
  tokens: Token[];
  kind: "subshell" | "substitution" | "backtick";
}
interface RedirectToken {
  type: "redirect";
  op: string; // ">", ">>", "<", "<<", "<<-", "<<<", or with fd: "2>", "2>>"
  target: string; // filename, heredoc delimiter, or here-string value
}
export type Token = WordToken | OpToken | GroupToken | RedirectToken;

export function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  const len = input.length;

  const pendingHeredocs: { delimiter: string; strip: boolean }[] = [];

  function readRedirectTarget(): string {
    while (i < len && (input[i] === " " || input[i] === "\t")) i++;
    if (i >= len || input[i] === "\n" || input[i] === "\r") return "";
    if (input[i] === "'") return readSingleQuoted();
    if (input[i] === '"') return readDoubleQuoted();
    let target = "";
    while (
      i < len &&
      input[i] !== " " &&
      input[i] !== "\t" &&
      input[i] !== "\n" &&
      input[i] !== "\r" &&
      input[i] !== "|" &&
      input[i] !== ";" &&
      input[i] !== "&" &&
      input[i] !== ")" &&
      input[i] !== "#"
    ) {
      if (input[i] === "\\") {
        i++;
        if (i < len) target += input[i++];
        continue;
      }
      target += input[i++];
    }
    return target;
  }

  function consumeHeredocBodies(): void {
    for (const hd of pendingHeredocs) {
      while (i < len) {
        const lineStart = i;
        while (i < len && input[i] !== "\n") i++;
        let line = input.substring(lineStart, i);
        if (i < len) i++; // consume newline
        if (hd.strip) {
          let j = 0;
          while (j < line.length && line[j] === "\t") j++;
          line = line.substring(j);
        }
        if (line === hd.delimiter) break;
      }
    }
    pendingHeredocs.length = 0;
  }

  function peek(offset = 0): string {
    return i + offset < len ? input[i + offset] : "";
  }

  function readSingleQuoted(): string {
    // consume opening '
    i++;
    let val = "";
    while (i < len && input[i] !== "'") {
      val += input[i++];
    }
    if (i >= len) throw new Error("Unmatched single quote");
    i++; // consume closing '
    return val;
  }

  function readDoubleQuoted(): string {
    // consume opening "
    i++;
    let val = "";
    while (i < len && input[i] !== '"') {
      const ch = input[i];
      if (ch === "\\") {
        i++;
        if (i < len) val += input[i++];
        continue;
      }
      if (ch === "$" && peek(1) === "(") {
        // command substitution inside double quotes — keep as text (already in a word)
        i += 2; // skip $(
        let depth = 1;
        let inner = "";
        while (i < len && depth > 0) {
          if (input[i] === "(") depth++;
          else if (input[i] === ")") {
            depth--;
            if (depth === 0) {
              i++;
              break;
            }
          }
          inner += input[i++];
        }
        if (depth !== 0) throw new Error("Unmatched $( inside double quote");
        // embed inner tokens as text (evaluated separately when we hit this word later)
        // For policy purposes: join inner content so wrapper detection works
        val += inner;
        continue;
      }
      if (ch === "`") {
        i++; // skip opening backtick
        let inner = "";
        while (i < len && input[i] !== "`") {
          if (input[i] === "\\") {
            i++;
            if (i < len) inner += input[i++];
            continue;
          }
          inner += input[i++];
        }
        if (i >= len) throw new Error("Unmatched backtick inside double quote");
        i++; // skip closing backtick
        val += inner;
        continue;
      }
      val += input[i++];
    }
    if (i >= len) throw new Error("Unmatched double quote");
    i++; // consume closing "
    return val;
  }

  function readSubstitution(): GroupToken {
    // i is at '$', next is '('
    i += 2; // skip $(
    let depth = 1;
    let inner = "";
    while (i < len && depth > 0) {
      if (input[i] === "(") depth++;
      else if (input[i] === ")") {
        depth--;
        if (depth === 0) {
          i++;
          break;
        }
      }
      inner += input[i++];
    }
    if (depth !== 0) throw new Error("Unmatched $(");
    return { type: "group", tokens: tokenize(inner), kind: "substitution" };
  }

  function readBacktick(): GroupToken {
    i++; // skip opening `
    let inner = "";
    while (i < len && input[i] !== "`") {
      if (input[i] === "\\") {
        i++;
        if (i < len) inner += input[i++];
        continue;
      }
      inner += input[i++];
    }
    if (i >= len) throw new Error("Unmatched backtick");
    i++; // skip closing `
    return { type: "group", tokens: tokenize(inner), kind: "backtick" };
  }

  function readSubshell(): GroupToken {
    i++; // skip opening (
    let depth = 1;
    let inner = "";
    while (i < len && depth > 0) {
      if (input[i] === "(") depth++;
      else if (input[i] === ")") {
        depth--;
        if (depth === 0) {
          i++;
          break;
        }
      }
      inner += input[i++];
    }
    if (depth !== 0) throw new Error("Unmatched (");
    return { type: "group", tokens: tokenize(inner), kind: "subshell" };
  }

  function readWord(): WordToken {
    let val = "";
    while (i < len) {
      const ch = input[i];
      if (ch === " " || ch === "\t" || ch === "\n" || ch === "\r") break;
      if (ch === "'") {
        val += readSingleQuoted();
        continue;
      }
      if (ch === '"') {
        val += readDoubleQuoted();
        continue;
      }
      if (ch === "#" && val === "") break; // comment at word boundary
      if (ch === "|" || ch === "&" || ch === ";") break;
      if (ch === "(" && val === "") break; // subshell, handled above
      if (ch === "`") break; // backtick group, handled above
      if (ch === "$" && peek(1) === "(") break; // substitution, handled above
      if (ch === "\\") {
        i++;
        if (i < len && input[i] === "\n") {
          i++;
          continue;
        } // line continuation
        if (i < len) val += input[i++];
        continue;
      }
      // Redirections: stop word here — handled by main loop
      if (ch === ">" || (ch === "<" && peek(1) !== "(")) break;
      val += input[i++];
    }
    return { type: "word", value: val };
  }

  while (i < len) {
    const ch = input[i];

    if (ch === " " || ch === "\t") {
      i++;
      continue;
    }
    if (ch === "\n" || ch === "\r") {
      i++;
      if (pendingHeredocs.length > 0) consumeHeredocBodies();
      continue;
    }

    // Comment - skip to end of line
    if (ch === "#") {
      while (i < len && input[i] !== "\n" && input[i] !== "\r") i++;
      continue;
    }

    // Operators: handle multi-char first
    if (ch === "|" && peek(1) === "|") {
      tokens.push({ type: "operator", value: "||" });
      i += 2;
      continue;
    }
    if (ch === "&" && peek(1) === "&") {
      tokens.push({ type: "operator", value: "&&" });
      i += 2;
      continue;
    }
    if (ch === "|") {
      tokens.push({ type: "operator", value: "|" });
      i++;
      continue;
    }
    if (ch === ";") {
      tokens.push({ type: "operator", value: ";" });
      i++;
      continue;
    }
    if (ch === "&" && peek(1) !== "&") {
      tokens.push({ type: "operator", value: "&" });
      i++;
      continue;
    }

    // Subshell
    if (ch === "(") {
      tokens.push(readSubshell());
      continue;
    }

    // Command substitution $( ... )
    if (ch === "$" && peek(1) === "(") {
      tokens.push(readSubstitution());
      continue;
    }

    // Backtick substitution
    if (ch === "`") {
      tokens.push(readBacktick());
      continue;
    }

    // Variable substitution $VAR — skip as a word fragment
    if (ch === "$") {
      i++;
      if (i < len && input[i] === "{") {
        // ${...} — skip to matching }
        i++;
        while (i < len && input[i] !== "}") i++;
        if (i < len) i++;
        continue;
      }
      // $VAR — skip identifier chars
      while (i < len && /\w/.test(input[i])) i++;
      continue;
    }

    // Redirections and heredocs
    let fdPrefix = "";
    let ri = i;
    if (ch >= "0" && ch <= "9" && (peek(1) === ">" || peek(1) === "<")) {
      fdPrefix = ch;
      ri = i + 1;
    }

    const rch = ri < len ? input[ri] : "";

    if (rch === "<" || rch === ">") {
      // Here-string: <<<
      if (
        rch === "<" &&
        ri + 1 < len &&
        input[ri + 1] === "<" &&
        ri + 2 < len &&
        input[ri + 2] === "<"
      ) {
        i = ri + 3;
        const target = readRedirectTarget();
        tokens.push({ type: "redirect", op: fdPrefix + "<<<", target });
        continue;
      }

      // Heredoc: << or <<-
      if (
        rch === "<" &&
        ri + 1 < len &&
        input[ri + 1] === "<" &&
        (ri + 2 >= len || input[ri + 2] !== "<")
      ) {
        i = ri + 2;
        let strip = false;
        if (i < len && input[i] === "-") {
          strip = true;
          i++;
        }
        while (i < len && (input[i] === " " || input[i] === "\t")) i++;
        let delim = "";
        if (i < len && (input[i] === "'" || input[i] === '"')) {
          const q = input[i++];
          while (i < len && input[i] !== q) delim += input[i++];
          if (i < len) i++;
        } else {
          while (
            i < len &&
            input[i] !== " " &&
            input[i] !== "\t" &&
            input[i] !== "\n" &&
            input[i] !== "\r" &&
            input[i] !== ";" &&
            input[i] !== "|" &&
            input[i] !== "&" &&
            input[i] !== ")" &&
            input[i] !== "#"
          ) {
            if (input[i] === "\\") {
              i++;
              if (i < len) delim += input[i++];
              continue;
            }
            delim += input[i++];
          }
        }
        const op = fdPrefix + (strip ? "<<-" : "<<");
        tokens.push({ type: "redirect", op, target: delim });
        pendingHeredocs.push({ delimiter: delim, strip });
        continue;
      }

      // Output: >> or >
      if (rch === ">") {
        i = ri + 1;
        let op = fdPrefix + ">";
        if (i < len && input[i] === ">") {
          op += ">";
          i++;
        }
        if (i < len && input[i] === "&") {
          i++;
          let fd = "";
          while (i < len && input[i] >= "0" && input[i] <= "9")
            fd += input[i++];
          tokens.push({
            type: "redirect",
            op: op + "&",
            target: fd || "-",
          });
          continue;
        }
        const target = readRedirectTarget();
        tokens.push({ type: "redirect", op, target });
        continue;
      }

      // Input: < (but not <( process substitution — not handled;
      // unrecognized syntax falls through to "ask" in the caller)
      if (rch === "<" && (ri + 1 >= len || input[ri + 1] !== "(")) {
        i = ri + 1;
        const target = readRedirectTarget();
        tokens.push({ type: "redirect", op: fdPrefix + "<", target });
        continue;
      }
    }

    // Word
    const word = readWord();
    if (word.value !== "") tokens.push(word);
  }

  pendingHeredocs.length = 0;

  return tokens;
}

// Command Extraction

/**
 * WRAPPER STRATEGY DESIGN
 *
 * Wrapper handling is semantics-based, not pattern-based. Different wrapper families
 * have different operand semantics according to official docs:
 *
 * 1. shell-c family (bash, sh, zsh, dash, ksh):
 *    - With -c option, the next operand is the command_string itself
 *    - Do NOT special-case -- after -c; if -- is the next operand, it IS the command
 *    - Example: bash -c -- 'rm -rf /' treats -- as the command string
 *
 * 2. utility-operand family (time, nohup, nice, timeout, setsid, chroot, sudo, doas):
 *    - Follow POSIX Guideline 10: -- ends option parsing
 *    - First remaining operand after options is the utility to invoke
 *    - Example: time -- echo hi extracts "echo" as the inner command
 *
 * 3. env-like family (env):
 *    - Follows -- end-of-options convention
 *    - Skips NAME=VALUE assignments
 *    - First remaining operand is the utility
 *    - Example: env -- VAR=1 cmd arg extracts "cmd"
 *
 * 4. xargs-like family (xargs):
 *    - Similar to utility-operand but may construct command from operands
 *    - Treated separately due to different command-construction semantics
 *
 * 5. docker-run family (docker, podman):
 *    - Recognizes subcommands: run, exec, create
 *    - Skips Docker/Podman flags (with arity-aware parsing) and image/container name
 *    - Uses -- end-of-options if present; otherwise best-effort flag parsing
 *    - Unknown boolean flags cause safe fallthrough (fail to extract → docker
 *      command itself evaluated by policy); false positives cannot occur
 *    - Note: docker compose run is NOT handled (subcommand would be "compose")
 *
 * WHY NO GENERIC -- HEURISTIC?
 *
 * A naive "anything after -- is a nested command" rule would be WRONG for:
 * - bash -c -- 'cmd': -- is the command string, not a delimiter
 * - grep -- -v file: -v is a literal operand, not a nested command
 *
 * Therefore, wrapper recursion is driven by a configurable strategy table
 * (WrapperRuleMap) that maps command names to their documented semantics.
 *
 * References:
 * - POSIX Utility Syntax Guidelines (Guideline 10):
 *   https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
 * - GNU Coreutils env invocation:
 *   https://www.gnu.org/software/coreutils/manual/html_node/env-invocation.html
 * - Bash -c invocation: https://man7.org/linux/man-pages/man1/bash.1.html
 * - POSIX time utility:
 *   https://pubs.opengroup.org/onlinepubs/9699919799/utilities/time.html
 * - POSIX nohup utility:
 *   https://pubs.opengroup.org/onlinepubs/9699919799/utilities/nohup.html
 */

export interface ExtractedCommand {
  name: string;
  fullText: string;
  words: string[];
  redirects: { op: string; target: string }[];
  source: "direct" | "subshell" | "substitution" | "wrapper-arg";
}

export type WrapperKind =
  | "shell-c"
  | "utility-operand"
  | "env"
  | "xargs"
  | "docker-run";

export interface WrapperRule {
  kind: WrapperKind;
}

export type WrapperRuleMap = ReadonlyMap<string, WrapperRule>;

// Evaluation Detail Types (for analyze()/evaluate())

export type DecisionSource = "commands" | "redirects" | "heredocs" | "default";

export interface CommandEvaluation {
  name: string;
  fullText: string;
  words: string[];
  source: ExtractedCommand["source"];
  redirects: { op: string; target: string }[];
  action: Action;
  match?: PolicyMatch;
  matches: PolicyMatch[];
}

export interface RedirectEvaluation {
  cmdName: string;
  op: string;
  target: string;
  action: Action;
  reason: string;
}

export interface HeredocEvaluation {
  detected: boolean;
  action: Action;
  reason: string;
}

export interface CommandsPhaseSummary {
  action: Action;
  reason: string;
  triggered: boolean;
  match?: PolicyMatch;
}

export interface RedirectsPhaseSummary {
  action: Action;
  reason: string;
  triggered: boolean;
  redirects: RedirectEvaluation[];
}

export interface HeredocsPhaseSummary {
  action: Action;
  reason: string;
  triggered: boolean;
  heredoc: HeredocEvaluation;
}

export interface ShellPolicyAnalysis {
  input: string;
  tokens: Token[];
  commands: CommandEvaluation[];
  phases: {
    commands?: CommandsPhaseSummary;
    redirects?: RedirectsPhaseSummary;
    heredocs?: HeredocsPhaseSummary;
  };
  final: {
    action: Action;
    reason: string;
    decidedBy: DecisionSource;
    match?: PolicyMatch;
  };
}

export interface EvaluationPolicy {
  commands?: PolicyCommands;
  redirects?: RedirectPolicy;
  heredocs?: HeredocPolicy;
  wrappers?: WrapperRuleConfig[];
}

/** Unified evaluation result from evaluate() */
export interface EvalResult {
  action: Action;
  reason: string;
  decidedBy: DecisionSource;
  match?: PolicyMatch;
  details?: {
    commands: CommandEvaluation[];
    redirects?: RedirectEvaluation[];
    heredocs?: HeredocEvaluation;
  };
}

const EMPTY_WRAPPER_RULES: WrapperRuleMap = new Map();

// Keywords that consume the entire segment (no embedded command)
const SHELL_KEYWORDS_SKIP_SEGMENT = new Set([
  "for",
  "case",
  "select",
  "in",
  "done",
  "fi",
  "esac",
]);

// Keywords to strip from the front of a segment to reach the actual command
const SHELL_KEYWORDS_STRIP = new Set([
  "while",
  "until",
  "if",
  "elif",
  "do",
  "then",
  "else",
]);

function isAssignmentToken(word: string): boolean {
  const eq = word.indexOf("=");
  return eq > 0;
}

function extractShellCInner(wordTokens: WordToken[]): WordToken[] | undefined {
  for (let i = 1; i + 1 < wordTokens.length; i++) {
    if (wordTokens[i].value === "-c") {
      return [wordTokens[i + 1]];
    }
  }
  return undefined;
}

function extractUtilityOperandInner(
  wordTokens: WordToken[],
): WordToken[] | undefined {
  let sawDoubleDash = false;
  for (let i = 1; i < wordTokens.length; i++) {
    const value = wordTokens[i].value;
    if (!sawDoubleDash && value === "--") {
      sawDoubleDash = true;
      continue;
    }
    if (!sawDoubleDash && value.startsWith("-")) {
      continue;
    }
    return wordTokens.slice(i);
  }
  return undefined;
}

function extractEnvInner(wordTokens: WordToken[]): WordToken[] | undefined {
  let sawDoubleDash = false;
  let i = 1;
  while (i < wordTokens.length) {
    const value = wordTokens[i].value;
    if (!sawDoubleDash && value === "--") {
      sawDoubleDash = true;
      i++;
      continue;
    }
    if (!sawDoubleDash && value.startsWith("-")) {
      i++;
      continue;
    }
    if (isAssignmentToken(value)) {
      i++;
      continue;
    }
    return wordTokens.slice(i);
  }
  return undefined;
}

function extractXargsInner(wordTokens: WordToken[]): WordToken[] | undefined {
  return extractUtilityOperandInner(wordTokens);
}

const DOCKER_BOOLEAN_FLAGS = new Set([
  "--detach",
  "--interactive",
  "--tty",
  "--rm",
  "--privileged",
  "--init",
  "--read-only",
  "--publish-all",
  "--oom-kill-disable",
  "--no-healthcheck",
  "--sig-proxy",
  "--help",
  "-d",
  "-i",
  "-t",
  "-P",
]);

function extractDockerRunInner(
  wordTokens: WordToken[],
): WordToken[] | undefined {
  if (wordTokens.length < 3) return undefined;
  const sub = wordTokens[1].value.toLowerCase();
  if (sub !== "run" && sub !== "exec" && sub !== "create") return undefined;

  let i = 2;
  while (i < wordTokens.length) {
    const val = wordTokens[i].value;

    // -- ends option parsing; next token is image/container, then command
    if (val === "--") {
      i += 2; // skip -- and image/container
      return i < wordTokens.length ? wordTokens.slice(i) : undefined;
    }

    // Long flag with = is self-contained
    if (val.startsWith("--") && val.includes("=")) {
      i++;
      continue;
    }

    // Known boolean flag (long or short)
    if (DOCKER_BOOLEAN_FLAGS.has(val)) {
      i++;
      continue;
    }

    // Combined short flags: -dit, -it, etc. (length > 2 = multiple flags)
    if (val.startsWith("-") && !val.startsWith("--") && val.length > 2) {
      i++;
      continue;
    }

    // Unknown long flag without = — assume takes next token as argument
    if (val.startsWith("--")) {
      i += 2;
      continue;
    }

    // Single short flag (-v, -e, -p, etc.) — assume takes next token as argument
    if (val.startsWith("-") && val.length === 2) {
      i += 2;
      continue;
    }

    // First non-flag token is image/container — skip it
    i++;
    // Remaining tokens are the command
    return i < wordTokens.length ? wordTokens.slice(i) : undefined;
  }
  return undefined;
}

function extractWrapperInnerTokens(
  rule: WrapperRule,
  wordTokens: WordToken[],
): WordToken[] | undefined {
  switch (rule.kind) {
    case "shell-c":
      return extractShellCInner(wordTokens);
    case "utility-operand":
      return extractUtilityOperandInner(wordTokens);
    case "env":
      return extractEnvInner(wordTokens);
    case "xargs":
      return extractXargsInner(wordTokens);
    case "docker-run":
      return extractDockerRunInner(wordTokens);
    default:
      return undefined;
  }
}

export function extractCommands(
  tokens: Token[],
  source: ExtractedCommand["source"] = "direct",
  wrapperRules: WrapperRuleMap = EMPTY_WRAPPER_RULES,
): ExtractedCommand[] {
  const results: ExtractedCommand[] = [];

  // Split on operators to get individual command segments
  const segments: Token[][] = [];
  let current: Token[] = [];
  for (const tok of tokens) {
    if (tok.type === "operator") {
      if (current.length > 0) segments.push(current);
      current = [];
    } else {
      current.push(tok);
    }
  }
  if (current.length > 0) segments.push(current);

  for (const seg of segments) {
    // Extract commands from group tokens (subshells/substitutions) first
    for (const tok of seg) {
      if (tok.type === "group") {
        const kind = tok.kind === "subshell" ? "subshell" : "substitution";
        results.push(...extractCommands(tok.tokens, kind, wrapperRules));
      }
    }

    // Find the command name: first non-keyword, non-group word token
    let wordTokens = seg.filter((t): t is WordToken => t.type === "word");
    const redirectTokens = seg.filter(
      (t): t is RedirectToken => t.type === "redirect",
    );
    if (wordTokens.length === 0) continue;
    // Skip segments that are entirely shell compound-command syntax
    if (SHELL_KEYWORDS_SKIP_SEGMENT.has(wordTokens[0].value)) continue;
    // Strip keywords that precede a command (while, if, do, then, etc.)
    if (SHELL_KEYWORDS_STRIP.has(wordTokens[0].value)) {
      wordTokens = wordTokens.slice(1);
    }
    if (wordTokens.length === 0) continue;

    const cmdName = wordTokens[0].value;
    if (!cmdName) continue;

    const fullText = wordTokens.map((t) => t.value).join(" ");
    const words = wordTokens.map((t) => t.value);
    const redirects = redirectTokens.map((t) => ({
      op: t.op,
      target: t.target,
    }));
    results.push({ name: cmdName, fullText, words, redirects, source });

    const wrapperRule = wrapperRules.get(cmdName.toLowerCase());
    if (wrapperRule) {
      const innerTokens = extractWrapperInnerTokens(wrapperRule, wordTokens);
      if (innerTokens && innerTokens.length > 0) {
        const innerText = innerTokens.map((t) => t.value).join(" ");
        try {
          results.push(
            ...extractCommands(
              tokenize(innerText),
              "wrapper-arg",
              wrapperRules,
            ),
          );
        } catch {
          // parse failure — the nested command will default to "ask"
        }
      }
    }
  }

  return results;
}

export function buildWrapperRuleMap(
  entries: WrapperRuleConfig[] | undefined,
): WrapperRuleMap {
  if (!entries || entries.length === 0) return EMPTY_WRAPPER_RULES;
  const map = new Map<string, WrapperRule>();
  for (const entry of entries) {
    map.set(entry.name.toLowerCase(), { kind: entry.kind });
  }
  return map;
}

// Policy Matching

interface ArgsPattern {
  programPrefix: string;
  requiredArgs: string[];
}

function parseArgsPattern(pattern: string): ArgsPattern {
  const colonIdx = pattern.indexOf(":");
  if (colonIdx === -1) {
    return { programPrefix: pattern, requiredArgs: [] };
  }
  const programPrefix = pattern.substring(0, colonIdx);
  const argsStr = pattern.substring(colonIdx + 1);
  const tokens = tokenize(argsStr);
  const requiredArgs = tokens
    .filter((t): t is WordToken => t.type === "word")
    .map((t) => t.value);
  return { programPrefix, requiredArgs };
}

function matchArgs(cmd: ExtractedCommand, pattern: ArgsPattern): boolean {
  if (pattern.programPrefix !== "*") {
    if (
      !cmd.fullText
        .trimStart()
        .toLowerCase()
        .startsWith(pattern.programPrefix.toLowerCase())
    ) {
      return false;
    }
  }
  const prefixWordCount =
    pattern.programPrefix === "*"
      ? 0
      : pattern.programPrefix.split(/\s+/).filter(Boolean).length;
  const cmdArgs = cmd.words.slice(prefixWordCount).map((w) => w.toLowerCase());
  return pattern.requiredArgs.every((req) =>
    cmdArgs.some((arg) => arg.toLowerCase() === req.toLowerCase()),
  );
}

function matchTokenSubstring(
  cmdWords: string[],
  matchTokens: string[],
): boolean {
  if (matchTokens.length === 0 || matchTokens.length > cmdWords.length)
    return false;
  outer: for (let i = 0; i <= cmdWords.length - matchTokens.length; i++) {
    for (let j = 0; j < matchTokens.length; j++) {
      if (cmdWords[i + j].toLowerCase() !== matchTokens[j].toLowerCase()) {
        continue outer;
      }
    }
    return true;
  }
  return false;
}

function matchEntry(cmd: ExtractedCommand, entry: CommandEntry): boolean {
  const { match, mode } = entry;
  const text = cmd.fullText;
  switch (mode) {
    case "exact":
      return text.trim().toLowerCase() === match.toLowerCase();
    case "prefix": {
      const lower = text.trimStart().toLowerCase();
      const lowerMatch = match.toLowerCase();
      return lower === lowerMatch || lower.startsWith(lowerMatch + " ");
    }
    case "substring": {
      const matchTokens = match.split(/\s+/).filter(Boolean);
      return matchTokenSubstring(cmd.words, matchTokens);
    }
    case "args": {
      const pattern = parseArgsPattern(match);
      return matchArgs(cmd, pattern);
    }
    default: {
      const _exhaustive: never = mode;
      return false;
    }
  }
}

function collectPolicyMatches(
  cmd: ExtractedCommand,
  policy: PolicyCommands,
): PolicyMatch[] {
  return [
    ...policy.deny
      .filter((entry) => matchEntry(cmd, entry))
      .map((entry): PolicyMatch => ({ category: "deny", entry })),
    ...policy.ask
      .filter((entry) => matchEntry(cmd, entry))
      .map((entry): PolicyMatch => ({ category: "ask", entry })),
    ...policy.allow
      .filter((entry) => matchEntry(cmd, entry))
      .map((entry): PolicyMatch => ({ category: "allow", entry })),
  ];
}

function buildCommandEvaluation(
  cmd: ExtractedCommand,
  action: Action,
  matches: PolicyMatch[],
  match?: PolicyMatch,
): CommandEvaluation {
  return {
    name: cmd.name,
    fullText: cmd.fullText,
    words: cmd.words,
    source: cmd.source,
    redirects: cmd.redirects,
    action,
    match,
    matches,
  };
}

// Internal Evaluation Functions (not exported)

interface CommandsInternalResult extends CommandsPhaseSummary {
  commands: CommandEvaluation[];
}

function buildDefaultCommandEvaluations(
  cmds: ExtractedCommand[],
): CommandEvaluation[] {
  return cmds.map((cmd) => buildCommandEvaluation(cmd, "default", []));
}

function evaluateCommandsInternal(
  cmds: ExtractedCommand[],
  policy: PolicyCommands,
): CommandsInternalResult {
  const commandEvaluations: CommandEvaluation[] = [];
  let result: Action = "default";
  let sawDirectUnmatched = false;
  let matchInfo: PolicyMatch | undefined;

  for (const cmd of cmds) {
    let cmdAction: Action = "default";
    let cmdMatch: PolicyMatch | undefined;
    const matches = collectPolicyMatches(cmd, policy);
    const denyMatch = matches.find((match) => match.category === "deny");
    const askMatch = matches.find((match) => match.category === "ask");
    const allowMatch = matches.find((match) => match.category === "allow");

    if (denyMatch) {
      commandEvaluations.push(
        buildCommandEvaluation(cmd, "deny", matches, denyMatch),
      );
      return {
        action: "deny",
        reason: `Denied command: ${cmd.name}`,
        triggered: true,
        match: denyMatch,
        commands: commandEvaluations,
      };
    }

    if (askMatch) {
      result = "ask";
      matchInfo = askMatch;
      cmdAction = "ask";
      cmdMatch = askMatch;
    }

    if (allowMatch) {
      if (
        result === "default" &&
        (cmd.source === "wrapper-arg" || !sawDirectUnmatched)
      ) {
        result = "allow";
        matchInfo = allowMatch;
      }
      if (cmdAction === "default") {
        cmdAction = "allow";
        cmdMatch = allowMatch;
      }
    }

    if (cmdAction === "default") {
      if (cmd.source === "direct") sawDirectUnmatched = true;
      if (result === "allow") result = "default";
    }

    commandEvaluations.push(
      buildCommandEvaluation(cmd, cmdAction, matches, cmdMatch),
    );
  }

  return {
    action: result,
    reason:
      result === "default" ? "No policy match" : `Policy matched: ${result}`,
    triggered: result !== "default",
    match: matchInfo,
    commands: commandEvaluations,
  };
}

interface RedirectsInternalResult extends RedirectsPhaseSummary {}

function evaluateRedirectsInternal(
  cmds: ExtractedCommand[],
  policy: RedirectPolicy,
): RedirectsInternalResult {
  const redirectEvaluations: RedirectEvaluation[] = [];
  let triggered = false;
  let lastTriggeredReason = "No unsafe redirects";

  for (const cmd of cmds) {
    for (const r of cmd.redirects) {
      const baseOp = r.op.replace(/^\d/, "");
      if (
        baseOp === "<" ||
        baseOp === "<<" ||
        baseOp === "<<-" ||
        baseOp === "<<<"
      ) {
        redirectEvaluations.push({
          cmdName: cmd.name,
          op: r.op,
          target: r.target,
          action: "allow",
          reason: "Input redirect allowed",
        });
        continue;
      }

      if (policy.allowFdDup && r.op.endsWith("&")) {
        redirectEvaluations.push({
          cmdName: cmd.name,
          op: r.op,
          target: r.target,
          action: "allow",
          reason: "FD duplication allowed",
        });
        continue;
      }

      if (policy.safeTargets?.includes(r.target)) {
        redirectEvaluations.push({
          cmdName: cmd.name,
          op: r.op,
          target: r.target,
          action: "allow",
          reason: "Safe target",
        });
        continue;
      }

      if (policy.action !== "allow") {
        triggered = true;
      }
      lastTriggeredReason = `Redirect to "${r.target}"`;
      redirectEvaluations.push({
        cmdName: cmd.name,
        op: r.op,
        target: r.target,
        action: policy.action,
        reason: lastTriggeredReason,
      });

      if (policy.action !== "allow") {
        return {
          action: policy.action,
          reason: lastTriggeredReason,
          triggered,
          redirects: redirectEvaluations,
        };
      }
    }
  }

  return {
    action: "allow",
    reason: triggered ? lastTriggeredReason : "No unsafe redirects",
    triggered,
    redirects: redirectEvaluations,
  };
}

interface HeredocsInternalResult extends HeredocsPhaseSummary {}

function evaluateHeredocsInternal(
  cmds: ExtractedCommand[],
  policy: HeredocPolicy,
): HeredocsInternalResult {
  for (const cmd of cmds) {
    if (cmd.redirects.some((r) => r.op === "<<" || r.op === "<<-")) {
      return {
        action: policy.action,
        reason: "Heredoc detected",
        triggered: policy.action !== "allow",
        heredoc: {
          detected: true,
          action: policy.action,
          reason: "Heredoc detected",
        },
      };
    }
  }

  return {
    action: "allow",
    reason: "No heredocs",
    triggered: false,
    heredoc: {
      detected: false,
      action: "allow",
      reason: "No heredocs",
    },
  };
}

// Priority Determination Helper

const ACTION_PRIORITY: Record<Action, number> = {
  default: 0,
  allow: 1,
  ask: 2,
  deny: 3,
};

export function compareActions(left: Action, right: Action): number {
  return ACTION_PRIORITY[left] - ACTION_PRIORITY[right];
}

const SOURCE_PRIORITY: Record<Exclude<DecisionSource, "default">, number> = {
  commands: 0,
  redirects: 1,
  heredocs: 2,
};

interface WinnerCandidate {
  action: Action;
  reason: string;
  source: Exclude<DecisionSource, "default">;
  triggered: boolean;
  match?: PolicyMatch;
}

interface WinnerResult {
  action: Action;
  reason: string;
  source: DecisionSource;
  match?: PolicyMatch;
}

function compareCandidates(a: WinnerCandidate, b: WinnerCandidate): number {
  const actionDiff = compareActions(a.action, b.action);
  if (actionDiff !== 0) return actionDiff;
  return SOURCE_PRIORITY[b.source] - SOURCE_PRIORITY[a.source];
}

function determineWinner(
  commandResult: CommandsInternalResult | null,
  redirectResult: RedirectsInternalResult | null,
  heredocResult: HeredocsInternalResult | null,
): WinnerResult {
  const candidates: WinnerCandidate[] = [];

  if (commandResult) {
    candidates.push({
      action: commandResult.action,
      reason: commandResult.reason,
      source: "commands",
      triggered: commandResult.triggered,
      match: commandResult.match,
    });
  }

  if (redirectResult) {
    candidates.push({
      action: redirectResult.action,
      reason: redirectResult.reason,
      source: "redirects",
      triggered: redirectResult.triggered,
    });
  }

  if (heredocResult) {
    candidates.push({
      action: heredocResult.action,
      reason: heredocResult.reason,
      source: "heredocs",
      triggered: heredocResult.triggered,
    });
  }

  if (candidates.length === 0) {
    return {
      action: "default",
      reason: "No policy configured",
      source: "default",
    };
  }

  const activeCandidates = candidates.filter(
    (candidate) => candidate.triggered,
  );
  if (activeCandidates.length === 0) {
    return {
      action: "default",
      reason: commandResult?.reason ?? "No policy match",
      source: "default",
    };
  }

  const winner = activeCandidates.reduce((best, candidate) =>
    compareCandidates(candidate, best) > 0 ? candidate : best,
  );

  return {
    action: winner.action,
    reason: winner.reason,
    source: winner.source,
    match: winner.match,
  };
}

// Primary Evaluation Functions

export function analyze(
  command: string,
  policy: EvaluationPolicy,
): ShellPolicyAnalysis {
  const wrapperRules = buildWrapperRuleMap(policy.wrappers);

  let tokens: Token[];
  let cmds: ExtractedCommand[];
  try {
    tokens = tokenize(command);
    cmds = extractCommands(tokens, "direct", wrapperRules);
  } catch (e) {
    return {
      input: command,
      tokens: [],
      commands: [],
      phases: {},
      final: {
        action: "ask",
        reason: `Unparseable: ${String(e)}`,
        decidedBy: "default",
      },
    };
  }

  if (cmds.length === 0) {
    return {
      input: command,
      tokens,
      commands: [],
      phases: {},
      final: {
        action: "ask",
        reason: "Empty or unrecognized command",
        decidedBy: "default",
      },
    };
  }

  const commandResult = policy.commands
    ? evaluateCommandsInternal(cmds, policy.commands)
    : null;
  const redirectResult = policy.redirects
    ? evaluateRedirectsInternal(cmds, policy.redirects)
    : null;
  const heredocResult = policy.heredocs
    ? evaluateHeredocsInternal(cmds, policy.heredocs)
    : null;
  const winner = determineWinner(commandResult, redirectResult, heredocResult);
  const match = winner.source === "commands" ? commandResult?.match : undefined;

  return {
    input: command,
    tokens,
    commands: commandResult?.commands ?? buildDefaultCommandEvaluations(cmds),
    phases: {
      commands: commandResult
        ? {
            action: commandResult.action,
            reason: commandResult.reason,
            triggered: commandResult.triggered,
            match: commandResult.match,
          }
        : undefined,
      redirects: redirectResult ?? undefined,
      heredocs: heredocResult ?? undefined,
    },
    final: {
      action: winner.action,
      reason: winner.reason,
      decidedBy: winner.source,
      match,
    },
  };
}

export function evaluate(
  command: string,
  policy: EvaluationPolicy,
): EvalResult {
  const analysis = analyze(command, policy);
  return {
    action: analysis.final.action,
    reason: analysis.final.reason,
    decidedBy: analysis.final.decidedBy,
    match: analysis.final.match,
    details: {
      commands: analysis.commands,
      redirects: analysis.phases.redirects?.redirects,
      heredocs: analysis.phases.heredocs?.heredoc,
    },
  };
}

export function getCommandSummary(command: string): string {
  return command.length > 80 ? command.slice(0, 77) + "..." : command;
}

export function mergePolicies(...policies: PolicyCommands[]): PolicyCommands {
  return mergePoliciesStrict(...policies);
}

export function mergePoliciesStrict(
  ...policies: PolicyCommands[]
): PolicyCommands {
  return {
    allow: policies.flatMap((p) => p.allow),
    ask: policies.flatMap((p) => p.ask),
    deny: policies.flatMap((p) => p.deny),
  };
}

const actionRank: Record<Action, number> = {
  default: 0,
  allow: 1,
  ask: 2,
  deny: 3,
};

function stricterAction(
  a: Action | undefined,
  b: Action | undefined,
): Action | undefined {
  if (!a) return b;
  if (!b) return a;
  return actionRank[a] >= actionRank[b] ? a : b;
}

function mergeRedirectPoliciesStrict(
  ...policies: (RedirectPolicy | undefined)[]
): RedirectPolicy | undefined {
  return policies.reduce<RedirectPolicy | undefined>((acc, policy) => {
    if (!policy) return acc;
    if (!acc) return policy;
    return {
      ...acc,
      ...policy,
      action: stricterAction(acc.action, policy.action) ?? "allow",
      safeTargets: policy.safeTargets ?? acc.safeTargets,
      allowFdDup:
        acc.allowFdDup === false || policy.allowFdDup === false
          ? false
          : (policy.allowFdDup ?? acc.allowFdDup),
    };
  }, undefined);
}

function mergeHeredocPoliciesStrict(
  ...policies: (HeredocPolicy | undefined)[]
): HeredocPolicy | undefined {
  return policies.reduce<HeredocPolicy | undefined>((acc, policy) => {
    if (!policy) return acc;
    if (!acc) return policy;
    return {
      action: stricterAction(acc.action, policy.action) ?? "allow",
    };
  }, undefined);
}

export function mergeEvaluationPolicyStackStrict(
  defaultPolicy: ModePolicy,
  modePolicies: ModePolicy[],
): EvaluationPolicy {
  const policies = [defaultPolicy, ...modePolicies];
  return {
    commands: mergePoliciesStrict(...policies.map((p) => p.commands)),
    redirects: mergeRedirectPoliciesStrict(...policies.map((p) => p.redirects)),
    heredocs: mergeHeredocPoliciesStrict(...policies.map((p) => p.heredocs)),
    wrappers: policies.flatMap((p) => p.wrappers ?? []),
  };
}

/**
 * Merge a mode-specific policy on top of a default policy.
 *
 * - commands: concatenated (evaluation priority handles conflicts)
 * - redirects/heredocs: mode overrides default (if specified)
 * - wrappers: mode replaces default (if present; default.nix pre-merges)
 */
export function mergeEvaluationPolicies(
  defaultPolicy: ModePolicy,
  modePolicy?: ModePolicy,
): EvaluationPolicy {
  return mergeEvaluationPolicyStackStrict(
    defaultPolicy,
    modePolicy ? [modePolicy] : [],
  );
}
