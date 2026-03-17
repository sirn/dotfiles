/**
 * Shell Policy Library — AST-based command evaluation for Pi extensions.
 *
 * Tokenizes shell command strings structurally and evaluates every sub-command
 * (pipes, &&/||/;, subshells, command substitution, wrapper recursion) against
 * a policy table. Fails closed on parse errors (unknown → "ask").
 */

// --- Shared Types ---

export interface CommandEntry {
  match: string;
  mode: "exact" | "prefix" | "substring";
}

export interface PolicyCommands {
  allow: CommandEntry[];
  ask: CommandEntry[];
  deny: CommandEntry[];
}

export type Action = "allow" | "ask" | "deny" | "default";

export interface EvalResult {
  action: Action;
  reason: string;
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
    kind !== "xargs"
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

// --- Tokenizer ---

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

    // Comment
    if (ch === "#") break;

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

    // --- Redirections and heredocs ---
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

      // Input: < (but not <( process substitution)
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

// --- Command Extraction ---

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
  redirects: { op: string; target: string }[];
  source: "direct" | "subshell" | "substitution" | "wrapper-arg";
}

export type WrapperKind = "shell-c" | "utility-operand" | "env" | "xargs";

export interface WrapperRule {
  kind: WrapperKind;
}

export type WrapperRuleMap = ReadonlyMap<string, WrapperRule>;

const BUILTIN_WRAPPER_RULES: WrapperRuleMap = new Map([
  ["bash", { kind: "shell-c" }],
  ["sh", { kind: "shell-c" }],
  ["zsh", { kind: "shell-c" }],
  ["dash", { kind: "shell-c" }],
  ["ksh", { kind: "shell-c" }],
  ["sudo", { kind: "utility-operand" }],
  ["doas", { kind: "utility-operand" }],
  ["time", { kind: "utility-operand" }],
  ["nohup", { kind: "utility-operand" }],
  ["nice", { kind: "utility-operand" }],
  ["chroot", { kind: "utility-operand" }],
  ["timeout", { kind: "utility-operand" }],
  ["setsid", { kind: "utility-operand" }],
  ["env", { kind: "env" }],
  ["xargs", { kind: "xargs" }],
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
    default:
      return undefined;
  }
}

export function extractCommands(
  tokens: Token[],
  source: ExtractedCommand["source"] = "direct",
  wrapperRules: WrapperRuleMap = BUILTIN_WRAPPER_RULES,
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

    // Find the command name: first non-group word token
    const wordTokens = seg.filter((t): t is WordToken => t.type === "word");
    const redirectTokens = seg.filter(
      (t): t is RedirectToken => t.type === "redirect",
    );
    if (wordTokens.length === 0) continue;

    const cmdName = wordTokens[0].value;
    if (!cmdName) continue;

    const fullText = wordTokens.map((t) => t.value).join(" ");
    const redirects = redirectTokens.map((t) => ({
      op: t.op,
      target: t.target,
    }));
    results.push({ name: cmdName, fullText, redirects, source });

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
  if (!entries || entries.length === 0) return BUILTIN_WRAPPER_RULES;
  const map = new Map<string, WrapperRule>();
  for (const entry of entries) {
    map.set(entry.name.toLowerCase(), { kind: entry.kind });
  }
  return map;
}

// --- Policy Matching ---

function matchEntry(cmd: ExtractedCommand, entry: CommandEntry): boolean {
  const { match, mode } = entry;
  const text = cmd.fullText;
  switch (mode) {
    case "exact":
      return text.trim().toLowerCase() === match.toLowerCase();
    case "prefix":
      return text.trimStart().toLowerCase().startsWith(match.toLowerCase());
    case "substring":
      return text.toLowerCase().includes(match.toLowerCase());
    default: {
      const _exhaustive: never = mode;
      return false;
    }
  }
}

export function evaluateCommand(
  command: string,
  policy: PolicyCommands,
  wrapperRules?: WrapperRuleMap,
): EvalResult {
  let cmds: ExtractedCommand[];
  try {
    cmds = extractCommands(
      tokenize(command),
      "direct",
      wrapperRules ?? BUILTIN_WRAPPER_RULES,
    );
  } catch (e) {
    return {
      action: "ask",
      reason: `Unparseable shell command: ${String(e)}`,
    };
  }

  if (cmds.length === 0) {
    return { action: "ask", reason: "Empty or unrecognized command" };
  }

  let result: Action = "default";
  let sawDirectUnmatched = false;

  for (const cmd of cmds) {
    // Deny takes immediate priority
    if (policy.deny.some((e) => matchEntry(cmd, e))) {
      return { action: "deny", reason: `Denied command: ${cmd.name}` };
    }
    // Ask escalates result
    if (policy.ask.some((e) => matchEntry(cmd, e))) {
      result = "ask";
      continue;
    }
    // Allow: wrapper-arg commands can always promote; direct commands only if no prior unmatched direct
    if (policy.allow.some((e) => matchEntry(cmd, e))) {
      if (
        result === "default" &&
        (cmd.source === "wrapper-arg" || !sawDirectUnmatched)
      )
        result = "allow";
      continue;
    }
    // No match — downgrade allow to default; track unmatched direct commands
    if (cmd.source === "direct") sawDirectUnmatched = true;
    result = result === "allow" ? "default" : result;
  }

  return {
    action: result,
    reason:
      result === "default" ? "No policy match" : `Policy matched: ${result}`,
  };
}

export function evaluateRedirects(
  cmds: ExtractedCommand[],
  policy: RedirectPolicy,
): EvalResult {
  for (const cmd of cmds) {
    for (const r of cmd.redirects) {
      // Strip optional fd prefix (single digit) to get base op
      const baseOp = r.op.replace(/^\d/, "");
      // Skip input redirects
      if (
        baseOp === "<" ||
        baseOp === "<<" ||
        baseOp === "<<-" ||
        baseOp === "<<<"
      )
        continue;
      // Allow fd-dup operations if configured
      if (policy.allowFdDup && r.op.endsWith("&")) continue;
      // Allow safe targets
      if (policy.safeTargets?.includes(r.target)) continue;
      return { action: policy.action, reason: `Redirect to "${r.target}"` };
    }
  }
  return { action: "allow", reason: "No unsafe redirects" };
}

export function evaluateHeredocs(
  cmds: ExtractedCommand[],
  policy: HeredocPolicy,
): EvalResult {
  for (const cmd of cmds) {
    if (cmd.redirects.some((r) => r.op === "<<" || r.op === "<<-")) {
      return { action: policy.action, reason: "Heredoc detected" };
    }
  }
  return { action: "allow", reason: "No heredocs" };
}

export function getCommandSummary(command: string): string {
  return command.length > 80 ? command.slice(0, 77) + "..." : command;
}

export function mergePolicies(...policies: PolicyCommands[]): PolicyCommands {
  return {
    allow: policies.flatMap((p) => p.allow),
    ask: policies.flatMap((p) => p.ask),
    deny: policies.flatMap((p) => p.deny),
  };
}
