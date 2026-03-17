/**
 * Comprehensive Test Suite for Shell Policy Engine
 * Run with: nix run nixpkgs#tsx -- shell-policy.test.ts
 */

import {
  tokenize,
  extractCommands,
  evaluateCommand,
  evaluateRedirects,
  evaluateHeredocs,
  mergePolicies,
  buildWrapperRuleMap,
  normalizeShellPolicyConfig,
  normalizeUnifiedPolicyConfig,
  type PolicyCommands,
  type EvalResult,
  type WrapperRuleConfig,
  type ExtractedCommand,
  type RedirectPolicy,
  type HeredocPolicy,
} from "../lib/shell-policy.ts";

// Simple assertion framework (self-contained)
interface TestStats {
  passed: number;
  failed: number;
  failures: string[];
}

const stats: TestStats = { passed: 0, failed: 0, failures: [] };

function test(name: string, fn: () => void): void {
  try {
    fn();
    stats.passed++;
    console.log(`✓ ${name}`);
  } catch (e) {
    stats.failed++;
    const msg = e instanceof Error ? e.message : String(e);
    stats.failures.push(`${name}: ${msg}`);
    console.log(`✗ ${name}`);
    console.log(`  Error: ${msg}`);
  }
}

function assertEquals(actual: unknown, expected: unknown, msg?: string): void {
  const actualStr = JSON.stringify(actual);
  const expectedStr = JSON.stringify(expected);
  if (actualStr !== expectedStr) {
    throw new Error(msg || `Expected ${expectedStr}, got ${actualStr}`);
  }
}

function assertTrue(value: boolean, msg?: string): void {
  if (!value) {
    throw new Error(msg || `Expected true, got ${value}`);
  }
}

function assertThrows(fn: () => void, msg?: string): Error {
  try {
    fn();
    throw new Error(msg || "Expected function to throw");
  } catch (e) {
    if (
      e instanceof Error &&
      e.message === (msg || "Expected function to throw")
    ) {
      throw e;
    }
    return e as Error;
  }
}

// ==================== TOKENIZER TESTS ====================
console.log("\n=== Tokenizer Tests ===");

// Basic words
test("basic word", () => {
  const tokens = tokenize("ls");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0], { type: "word", value: "ls" });
});

test("multiple words", () => {
  const tokens = tokenize("ls -la");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[0], { type: "word", value: "ls" });
  assertEquals(tokens[1], { type: "word", value: "-la" });
});

test("echo with arguments", () => {
  const tokens = tokenize("echo hello world");
  assertEquals(tokens.length, 3);
  assertEquals(tokens[0], { type: "word", value: "echo" });
  assertEquals(tokens[1], { type: "word", value: "hello" });
  assertEquals(tokens[2], { type: "word", value: "world" });
});

// Single quotes
test("single quoted string", () => {
  const tokens = tokenize("echo 'hello world'");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[0], { type: "word", value: "echo" });
  assertEquals(tokens[1], { type: "word", value: "hello world" });
});

test("escaped single quote (single quotes don't escape)", () => {
  assertThrows(() => tokenize("'it\\'s'"));
});

test("multiple single quoted words", () => {
  const tokens = tokenize("'foo' 'bar'");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[0], { type: "word", value: "foo" });
  assertEquals(tokens[1], { type: "word", value: "bar" });
});

// Double quotes
test("double quoted string", () => {
  const tokens = tokenize('echo "hello world"');
  assertEquals(tokens.length, 2);
  assertEquals(tokens[1], { type: "word", value: "hello world" });
});

test("escaped double quote", () => {
  const tokens = tokenize('"say \\"hi\\""');
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0], { type: "word", value: 'say "hi"' });
});

test("double quote with variable reference", () => {
  const tokens = tokenize('"value is $VAR"');
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0], { type: "word", value: "value is $VAR" });
});

// Escape sequences
test("escaped space", () => {
  const tokens = tokenize("hello\\ world");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0], { type: "word", value: "hello world" });
});

test("escaped newline (line continuation)", () => {
  const tokens = tokenize("echo \\\n  hello");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[0], { type: "word", value: "echo" });
  assertEquals(tokens[1], { type: "word", value: "hello" });
});

// Operators
test("pipe operator", () => {
  const tokens = tokenize("cat file | grep hi");
  assertEquals(tokens.length, 5);
  assertEquals(tokens[2], { type: "operator", value: "|" });
});

test("logical AND operator", () => {
  const tokens = tokenize("cmd1 && cmd2");
  assertEquals(tokens.length, 3);
  assertEquals(tokens[1], { type: "operator", value: "&&" });
});

test("logical OR operator", () => {
  const tokens = tokenize("cmd1 || cmd2");
  assertEquals(tokens.length, 3);
  assertEquals(tokens[1], { type: "operator", value: "||" });
});

test("semicolon operator", () => {
  const tokens = tokenize("cmd1 ; cmd2");
  assertEquals(tokens.length, 3);
  assertEquals(tokens[1], { type: "operator", value: ";" });
});

test("background operator", () => {
  const tokens = tokenize("cmd &");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[1], { type: "operator", value: "&" });
});

test("mixed operators", () => {
  const tokens = tokenize("a && b || c ; d &");
  assertEquals(tokens.filter((t) => t.type === "operator").length, 4);
});

// Groups - subshells
test("subshell group", () => {
  const tokens = tokenize("(echo hi)");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, "group");
});

test("subshell with multiple commands", () => {
  const tokens = tokenize("(echo a && echo b)");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, "group");
});

// Groups - command substitution
test("dollar substitution", () => {
  const tokens = tokenize("$(echo hi)");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, "group");
});

test("backtick substitution", () => {
  const tokens = tokenize("`echo hi`");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, "group");
});

test("nested substitution", () => {
  const tokens = tokenize("$(echo $(echo hi))");
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, "group");
});

// Redirections
test("output redirect", () => {
  const tokens = tokenize("echo hi > file.txt");
  assertEquals(tokens.length, 3);
  assertEquals(tokens[2], { type: "redirect", op: ">", target: "file.txt" });
});

test("append redirect", () => {
  const tokens = tokenize("echo hi >> file.txt");
  assertEquals(tokens[2], { type: "redirect", op: ">>", target: "file.txt" });
});

test("input redirect", () => {
  const tokens = tokenize("cat < file.txt");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[1], { type: "redirect", op: "<", target: "file.txt" });
});

test("heredoc redirect", () => {
  const tokens = tokenize("cat <<EOF");
  assertEquals(tokens[1], { type: "redirect", op: "<<", target: "EOF" });
});

test("heredoc with strip redirect", () => {
  const tokens = tokenize("cat <<-EOF");
  assertEquals(tokens[1], { type: "redirect", op: "<<-", target: "EOF" });
});

test("here-string redirect", () => {
  const tokens = tokenize("cat <<<'hello'");
  assertEquals(tokens[1], { type: "redirect", op: "<<<", target: "hello" });
});

test("fd output redirect", () => {
  const tokens = tokenize("cmd 2> file.txt");
  assertEquals(tokens[1], { type: "redirect", op: "2>", target: "file.txt" });
});

test("fd append redirect", () => {
  const tokens = tokenize("cmd 2>> file.txt");
  assertEquals(tokens[1], {
    type: "redirect",
    op: "2>>",
    target: "file.txt",
  });
});

test("fd duplication redirect", () => {
  const tokens = tokenize("cmd 2>&1");
  assertEquals(tokens[1], { type: "redirect", op: "2>&", target: "1" });
});

test("input duplication redirect (parsed as separate tokens)", () => {
  const tokens = tokenize("cmd <&0");
  assertEquals(tokens.length, 4);
  assertEquals(tokens[0], { type: "word", value: "cmd" });
  assertEquals(tokens[1], { type: "redirect", op: "<", target: "" });
  assertEquals(tokens[2], { type: "operator", value: "&" });
  assertEquals(tokens[3], { type: "word", value: "0" });
});

test("multiple redirects", () => {
  const tokens = tokenize("cmd > out.txt 2> err.txt");
  assertEquals(tokens.filter((t) => t.type === "redirect").length, 2);
});

// Comments
test("comment ignored", () => {
  const tokens = tokenize("echo hi # this is ignored");
  assertEquals(tokens.length, 2);
  assertEquals(tokens[0], { type: "word", value: "echo" });
  assertEquals(tokens[1], { type: "word", value: "hi" });
});

test("comment after redirect", () => {
  const tokens = tokenize("echo hi > file # comment");
  assertEquals(tokens.length, 3);
});

// Variable substitution
test("variable not parsed as standalone word", () => {
  const tokens = tokenize("echo $VAR");
  assertTrue(tokens.length >= 1);
  assertEquals(tokens[0], { type: "word", value: "echo" });
});

// Complex combinations
test("pipeline with redirects", () => {
  const tokens = tokenize(
    'echo "hello" > file.txt && cat < file.txt | grep hi',
  );
  assertEquals(tokens.length, 9);
  assertEquals(tokens.filter((t) => t.type === "operator").length, 2);
  assertEquals(tokens.filter((t) => t.type === "redirect").length, 2);
});

test("complex with groups", () => {
  const tokens = tokenize("echo $(date) && (ls -la) | wc -l");
  assertEquals(tokens.filter((t) => t.type === "group").length, 2);
});

// Error cases
test("throws on unmatched single quote", () => {
  assertThrows(() => tokenize("echo 'unclosed"));
});

test("throws on unmatched double quote", () => {
  assertThrows(() => tokenize('echo "unclosed'));
});

test("throws on unmatched subshell", () => {
  assertThrows(() => tokenize("(echo hi"));
});

test("throws on unmatched substitution", () => {
  assertThrows(() => tokenize("$(echo hi"));
});

test("throws on unmatched backtick", () => {
  assertThrows(() => tokenize("`echo hi"));
});

// ==================== END TOKENIZER TESTS ====================

// ==================== COMMAND EXTRACTION TESTS ====================
console.log("\n=== Command Extraction Tests ===");

// Basic commands
test("basic command", () => {
  const tokens = tokenize("ls -la");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "ls");
  assertEquals(cmds[0].fullText, "ls -la");
  assertEquals(cmds[0].source, "direct");
});

test("command with redirects", () => {
  const tokens = tokenize("echo hi > file.txt");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "echo");
  assertEquals(cmds[0].redirects.length, 1);
  assertEquals(cmds[0].redirects[0], { op: ">", target: "file.txt" });
});

test("command with multiple redirects", () => {
  const tokens = tokenize("cmd > out.txt 2> err.txt");
  const cmds = extractCommands(tokens);
  assertEquals(cmds[0].redirects.length, 2);
});

// Multiple segments with control operators
test("multiple with &&", () => {
  const tokens = tokenize("echo a && echo b");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "echo");
  assertEquals(cmds[0].fullText, "echo a");
  assertEquals(cmds[1].name, "echo");
  assertEquals(cmds[1].fullText, "echo b");
});

test("multiple with ||", () => {
  const tokens = tokenize("cmd1 || cmd2");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "cmd1");
  assertEquals(cmds[1].name, "cmd2");
});

test("multiple with ;", () => {
  const tokens = tokenize("cmd1 ; cmd2 ; cmd3");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 3);
});

// Pipelines
test("simple pipeline", () => {
  const tokens = tokenize("cat file | grep hi");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(
    cmds.map((c) => c.name),
    ["cat", "grep"],
  );
});

test("multi-stage pipeline", () => {
  const tokens = tokenize("cat file | grep hi | sort | uniq");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 4);
  assertEquals(
    cmds.map((c) => c.name),
    ["cat", "grep", "sort", "uniq"],
  );
});

// Subshells
test("subshell with single command", () => {
  const tokens = tokenize("(echo hi)");
  const cmds = extractCommands(tokens);
  const subshellCmds = cmds.filter((c) => c.source === "subshell");
  assertEquals(subshellCmds.length, 1);
  assertEquals(
    subshellCmds.map((c) => c.name),
    ["echo"],
  );
});

test("subshell with multiple commands", () => {
  const tokens = tokenize("(echo a && echo b)");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.filter((c) => c.source === "subshell").length, 2);
});

test("subshell with pipeline", () => {
  const tokens = tokenize("(cat file | grep hi)");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.filter((c) => c.source === "subshell").length, 2);
});

// Command substitution
test("command substitution $(...)", () => {
  const tokens = tokenize("echo $(date)");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  const cmd = cmds.find((c) => c.name === "date");
  assertTrue(cmd !== undefined);
  assertEquals(cmd?.source, "substitution");
});

test("backtick substitution", () => {
  const tokens = tokenize("echo `date`");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  const cmd = cmds.find((c) => c.name === "date");
  assertTrue(cmd !== undefined);
  assertEquals(cmd?.source, "substitution");
});

test("nested substitution", () => {
  const tokens = tokenize("echo $(echo $(date))");
  const cmds = extractCommands(tokens);
  assertTrue(cmds.length >= 2);
  assertTrue(cmds.some((c) => c.name === "date"));
});

// Wrapper commands
test("bash -c wrapper", () => {
  const tokens = tokenize("bash -c 'echo hi'");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  const cmd = cmds.find((c) => c.name === "echo");
  assertTrue(cmd !== undefined);
  assertEquals(cmd?.source, "wrapper-arg");
});

test("sh -c wrapper", () => {
  const tokens = tokenize("sh -c 'ls -la'");
  const cmds = extractCommands(tokens);
  const cmd = cmds.find((c) => c.name === "ls");
  assertTrue(cmd !== undefined);
  assertEquals(cmd?.source, "wrapper-arg");
});

test("sudo passthrough", () => {
  const tokens = tokenize("sudo rm -rf /");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "sudo");
  assertEquals(cmds[0].source, "direct");
  assertEquals(cmds[1].name, "rm");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("doas passthrough", () => {
  const tokens = tokenize("doas ls -la");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "ls");
});

test("xargs passthrough", () => {
  const tokens = tokenize("xargs rm");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "rm");
});

test("time passthrough without --", () => {
  const tokens = tokenize("time sleep 1");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "sleep");
});

test("time -- passthrough respects -- end-of-options", () => {
  const tokens = tokenize("time -- echo hi");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "echo");
});

test("env wrapper handles -- before command", () => {
  const tokens = tokenize("env -- VAR=val cmd arg");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cmd");
  assertEquals(cmds[1].fullText, "cmd arg");
});

test("env wrapper skips assignments", () => {
  const tokens = tokenize("env VAR=val echo hi");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "echo");
});

test("env with multiple assignments", () => {
  const tokens = tokenize("env A=1 B=2 C=3 cmd arg");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cmd");
  assertEquals(cmds[1].fullText, "cmd arg");
});

test("env skips assignments after double dash", () => {
  const tokens = tokenize("env -- KEY=val cmd");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cmd");
});

test("nohup wrapper extracts utility operand", () => {
  const tokens = tokenize("nohup cat file.txt");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cat");
});

test("sudo with only flags extracts no inner command", () => {
  const tokens = tokenize("sudo -h");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "sudo");
});

// Nested wrappers
test("nested wrappers bash -c with sudo", () => {
  const tokens = tokenize("sudo bash -c 'rm -rf /'");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 3);
  assertTrue(cmds.some((c) => c.name === "rm"));
});

test("deeply nested", () => {
  const tokens = tokenize("echo $(bash -c 'ls $(pwd)')");
  const cmds = extractCommands(tokens);
  assertTrue(cmds.length >= 3);
  assertTrue(cmds.some((c) => c.name === "bash"));
  assertTrue(cmds.some((c) => c.name === "ls"));
  assertTrue(cmds.some((c) => c.name === "pwd"));
});

// Complex cases
test("mixed pipeline and subshell", () => {
  const tokens = tokenize("(cat file) | grep hi");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "cat");
  assertEquals(cmds[0].source, "subshell");
  assertEquals(cmds[1].name, "grep");
  assertEquals(cmds[1].source, "direct");
});

test("command with empty segment after &&", () => {
  const tokens = tokenize("echo || ls");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
});

// ==================== END COMMAND EXTRACTION TESTS ====================

// ==================== POLICY MATCHING TESTS ====================
console.log("\n=== Policy Matching Tests ===");

// Helper for creating simple policies
const samplePolicy: PolicyCommands = {
  allow: [
    { match: "ls", mode: "prefix" },
    { match: "echo", mode: "prefix" },
  ],
  ask: [{ match: "rm", mode: "prefix" }],
  deny: [{ match: "sudo", mode: "prefix" }],
};

// Exact match mode
test("exact match - matches", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "ls", mode: "exact" }],
  };
  assertEquals(evaluateCommand("ls", policy).action, "deny");
});

test("exact match - case insensitive", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "LS", mode: "exact" }],
  };
  assertEquals(evaluateCommand("ls", policy).action, "deny");
});

test("exact match - whitespace trimmed", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "ls", mode: "exact" }],
  };
  assertEquals(evaluateCommand("  ls  ", policy).action, "deny");
});

test("exact match - args don't match", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "ls", mode: "exact" }],
  };
  assertEquals(evaluateCommand("ls -la", policy).action, "default");
});

// Prefix match mode
test("prefix match - matches", () => {
  assertEquals(evaluateCommand("ls -la", samplePolicy).action, "allow");
});

test("prefix match - exact also matches", () => {
  assertEquals(evaluateCommand("ls", samplePolicy).action, "allow");
});

test("prefix match - case insensitive", () => {
  assertEquals(evaluateCommand("LS -LA", samplePolicy).action, "allow");
});

test("prefix match - no match when pattern longer", () => {
  assertEquals(evaluateCommand("l", samplePolicy).action, "default");
});

// Substring match mode
test("substring match - exact", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(evaluateCommand("rm -rf /", policy).action, "deny");
});

test("substring match - in pipeline", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(evaluateCommand("echo hi && rm -rf /", policy).action, "deny");
});

test("substring match - partial word", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf", mode: "substring" }],
  };
  assertEquals(evaluateCommand("grm -rf file", policy).action, "deny");
});

test("substring match - case insensitive", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "RM -RF /", mode: "substring" }],
  };
  assertEquals(evaluateCommand("echo hi && rm -rf /", policy).action, "deny");
});

test("substring match - special characters", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "git push", mode: "substring" }],
  };
  assertEquals(evaluateCommand("git push origin main", policy).action, "deny");
});

// Priority tests
test("deny takes priority over allow", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [{ match: "ls", mode: "prefix" }],
  };
  assertEquals(evaluateCommand("ls -la", policy).action, "deny");
});

test("deny takes priority over ask", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "sudo", mode: "prefix" }],
    deny: [{ match: "sudo", mode: "prefix" }],
  };
  assertEquals(evaluateCommand("sudo ls", policy).action, "deny");
});

test("ask escalates over allow", () => {
  assertEquals(
    evaluateCommand("echo hi && rm file", samplePolicy).action,
    "ask",
  );
});

test("allow downgrades to default when later command unmatched", () => {
  assertEquals(
    evaluateCommand("echo hi && unknown_cmd", samplePolicy).action,
    "default",
  );
});

test("ask not downgraded when later command unmatched", () => {
  assertEquals(
    evaluateCommand("rm file && unknown_cmd", samplePolicy).action,
    "ask",
  );
});

test("allow-unmatched-allow does not re-grant allow", () => {
  assertEquals(
    evaluateCommand("echo hi | unknown_cmd | echo bye", samplePolicy).action,
    "default",
  );
});

test("allow-allow stays allow", () => {
  assertEquals(
    evaluateCommand("echo hi | echo bye", samplePolicy).action,
    "allow",
  );
});

test("unmatched-allow stays default", () => {
  assertEquals(
    evaluateCommand("unknown_cmd | echo hi", samplePolicy).action,
    "default",
  );
});

test("multiple commands - one deny triggers deny", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "echo", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(evaluateCommand("echo hi && rm -rf /", policy).action, "deny");
});

// Wrapper command extraction
test("wrapper command extraction - sudo denied", () => {
  assertEquals(evaluateCommand("sudo ls -la", samplePolicy).action, "deny");
});

test("wrapper command extraction - sudo prefix match", () => {
  assertEquals(evaluateCommand("sudo ls", samplePolicy).action, "deny");
});

test("wrapper command extraction - bash -c wrapper", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "echo", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm", mode: "prefix" }],
  };
  assertEquals(evaluateCommand("bash -c 'rm -rf /'", policy).action, "deny");
});

test("wrapper command extraction - nested wrappers", () => {
  assertEquals(
    evaluateCommand("sudo bash -c 'ls -la'", samplePolicy).action,
    "deny",
  );
});

// Error handling
test("parse error returns ask", () => {
  const result = evaluateCommand("echo 'unclosed", samplePolicy);
  assertEquals(result.action, "ask");
  assertTrue(result.reason?.includes("Unparseable"));
});

test("empty command returns ask", () => {
  assertEquals(evaluateCommand("", samplePolicy).action, "ask");
});

test("whitespace-only command returns ask", () => {
  assertEquals(evaluateCommand("   \n\t  ", samplePolicy).action, "ask");
});

test("unknown command returns default", () => {
  assertEquals(evaluateCommand("unknown_cmd", samplePolicy).action, "default");
});

test("unknown match mode treated as no-match", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "ls", mode: "nonexistent" as any }],
    ask: [],
    deny: [],
  };
  assertEquals(evaluateCommand("ls", policy).action, "default");
});

test("bare variable as command returns ask", () => {
  assertEquals(evaluateCommand("$CMD", samplePolicy).action, "ask");
});

test("${VAR} as command returns ask", () => {
  assertEquals(evaluateCommand("${CMD}", samplePolicy).action, "ask");
});

// ==================== END POLICY MATCHING TESTS ====================

// ==================== MERGE POLICIES TESTS ====================
console.log("\n=== Merge Policies Tests ===");

test("combines all sections", () => {
  const p1: PolicyCommands = {
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  const p2: PolicyCommands = {
    allow: [],
    ask: [{ match: "rm", mode: "prefix" }],
    deny: [{ match: "sudo", mode: "prefix" }],
  };
  const merged = mergePolicies(p1, p2);
  assertEquals(merged.allow.length, 1);
  assertEquals(merged.ask.length, 1);
  assertEquals(merged.deny.length, 1);
  assertEquals(merged.allow[0].match, "ls");
  assertEquals(merged.ask[0].match, "rm");
  assertEquals(merged.deny[0].match, "sudo");
});

test("combines multiple policies", () => {
  const merged = mergePolicies(
    { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [] },
    { allow: [{ match: "cat", mode: "prefix" }], ask: [], deny: [] },
    { allow: [{ match: "grep", mode: "prefix" }], ask: [], deny: [] },
  );
  assertEquals(merged.allow.length, 3);
  assertEquals(
    merged.allow.map((e) => e.match),
    ["ls", "cat", "grep"],
  );
});

test("empty policies", () => {
  const p1: PolicyCommands = { allow: [], ask: [], deny: [] };
  const p2: PolicyCommands = {
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  const merged = mergePolicies(p1, p2);
  assertEquals(merged.allow.length, 1);
  assertEquals(merged.ask.length, 0);
  assertEquals(merged.deny.length, 0);
});

test("both empty", () => {
  const p1: PolicyCommands = { allow: [], ask: [], deny: [] };
  const p2: PolicyCommands = { allow: [], ask: [], deny: [] };
  const merged = mergePolicies(p1, p2);
  assertEquals(merged.allow.length, 0);
  assertEquals(merged.ask.length, 0);
  assertEquals(merged.deny.length, 0);
});

test("order preservation", () => {
  const merged = mergePolicies(
    { allow: [{ match: "a", mode: "exact" }], ask: [], deny: [] },
    { allow: [{ match: "b", mode: "exact" }], ask: [], deny: [] },
    { allow: [{ match: "c", mode: "exact" }], ask: [], deny: [] },
  );
  assertEquals(
    merged.allow.map((e) => e.match),
    ["a", "b", "c"],
  );
});

// ==================== END MERGE POLICIES TESTS ====================

// ==================== EDGE CASE & REGRESSION TESTS ====================
console.log("\n=== Edge Cases & Regression Tests ===");

// Production policy from permissions.toml (actual real-world rules)
const productionPolicy: PolicyCommands = {
  allow: [
    { match: "ls", mode: "prefix" },
    { match: "echo", mode: "prefix" },
    { match: "cat", mode: "prefix" },
    { match: "grep", mode: "prefix" },
    { match: "rg", mode: "prefix" },
    { match: "find", mode: "prefix" },
  ],
  ask: [
    { match: "chmod", mode: "substring" },
    { match: "chown", mode: "substring" },
    { match: "rm", mode: "substring" },
    { match: "docker exec", mode: "substring" },
    { match: "nix run", mode: "substring" },
    { match: "jj describe", mode: "substring" },
  ],
  deny: [
    { match: "sudo", mode: "prefix" },
    { match: "doas", mode: "prefix" },
    { match: "git push", mode: "substring" },
    { match: "rm -rf /", mode: "substring" },
    { match: "gh api --method POST", mode: "substring" },
    { match: "gh api --method PUT", mode: "substring" },
    { match: "gh api --method DELETE", mode: "substring" },
  ],
};

// Production policy tests
test("git push denied", () => {
  assertEquals(
    evaluateCommand("git push origin main", productionPolicy).action,
    "deny",
  );
});
test("git status allowed", () => {
  assertEquals(
    evaluateCommand("git status", productionPolicy).action,
    "default",
  );
});
test("rm -rf / denied", () => {
  assertEquals(evaluateCommand("rm -rf /", productionPolicy).action, "deny");
});
test("rm -rf / in text denied", () => {
  assertEquals(
    evaluateCommand("echo hi && rm -rf /", productionPolicy).action,
    "deny",
  );
});
test("chmod asks", () => {
  assertEquals(
    evaluateCommand("chmod +x script.sh", productionPolicy).action,
    "ask",
  );
});
test("rm asks", () => {
  assertEquals(evaluateCommand("rm file.txt", productionPolicy).action, "ask");
});
test("docker exec asks", () => {
  assertEquals(
    evaluateCommand("docker exec -it container bash", productionPolicy).action,
    "ask",
  );
});
test("nix run asks", () => {
  assertEquals(
    evaluateCommand("nix run nixpkgs#something", productionPolicy).action,
    "ask",
  );
});
test("jj describe asks", () => {
  assertEquals(
    evaluateCommand("jj describe -m 'update'", productionPolicy).action,
    "ask",
  );
});
test("gh api POST pattern not matching", () => {
  assertEquals(
    evaluateCommand("gh api repos/foo --method POST", productionPolicy).action,
    "default",
  );
});
test("gh api GET allowed", () => {
  assertEquals(
    evaluateCommand("gh api repos/foo", productionPolicy).action,
    "default",
  );
});
test("sudo denied", () => {
  assertEquals(evaluateCommand("sudo ls -la", productionPolicy).action, "deny");
});
test("quoted > not redirect", () => {
  assertEquals(
    evaluateCommand("jq '.x > .y' file.json", productionPolicy).action,
    "default",
  );
});
test("echo with quoted > allowed", () => {
  assertEquals(
    evaluateCommand("echo 'hello > world'", productionPolicy).action,
    "allow",
  );
});
test("grep with quoted > allowed", () => {
  assertEquals(
    evaluateCommand("grep '>' file.txt", productionPolicy).action,
    "allow",
  );
});

// Regression tests
test("bash -c git push denied", () => {
  assertEquals(
    evaluateCommand("bash -c 'git push'", productionPolicy).action,
    "deny",
  );
});
test("echo with quoted rm -rf / - substring matches", () => {
  assertEquals(
    evaluateCommand("echo 'rm -rf /'", productionPolicy).action,
    "deny",
  );
});
test("grep with sudo pattern allowed", () => {
  assertEquals(
    evaluateCommand("grep 'sudo' file.txt", productionPolicy).action,
    "allow",
  );
});
test("bash -c rm -rf / denied", () => {
  assertEquals(
    evaluateCommand("bash -c 'rm -rf /'", productionPolicy).action,
    "deny",
  );
});
test("sudo in substitution denied", () => {
  assertEquals(
    evaluateCommand("$(sudo reboot)", productionPolicy).action,
    "deny",
  );
});

// Complex edge cases - tokenization/extraction
test("deeply nested subshells", () => {
  const tokens = tokenize("$(echo $(echo $(echo hi)))");
  const cmds = extractCommands(tokens);
  assertTrue(cmds.length >= 3);
});

test("wrapper in wrapper", () => {
  const tokens = tokenize("sudo bash -c 'sh -c \"echo hi\"'");
  const cmds = extractCommands(tokens);
  assertTrue(cmds.some((c) => c.name === "sudo"));
  assertTrue(cmds.some((c) => c.name === "bash"));
});

test("complex pipeline with subshells", () => {
  const tokens = tokenize("(cat a; cat b) | grep x | (sort | uniq)");
  const cmds = extractCommands(tokens);
  assertTrue(cmds.length >= 5);
});

test("multiple && || combinations", () => {
  const tokens = tokenize("cmd1 && cmd2 || cmd3 && cmd4");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 4);
});

test("long command chain", () => {
  const tokens = tokenize("a | b | c | d | e | f | g");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 7);
});

// Edge cases - redirects
test("redirects in various positions", () => {
  const tokens = tokenize("cmd > out 2> err < input");
  const cmds = extractCommands(tokens);
  assertEquals(cmds[0].redirects.length, 3);
});

test("quoted heredoc delimiter", () => {
  const tokens = tokenize("cat <<'EOF'\nhello\nEOF");
  assertEquals(tokens[1], { type: "redirect", op: "<<", target: "EOF" });
});

test("escaped characters in arguments", () => {
  const tokens = tokenize("echo 'foo*bar'");
  assertEquals(tokens[1], { type: "word", value: "foo*bar" });
});

test("double quotes with variable", () => {
  const tokens = tokenize('echo "value is $VAR"');
  assertEquals(tokens[1], { type: "word", value: "value is $VAR" });
});

// Edge cases - policy evaluation
test("command with flags before args", () => {
  assertEquals(
    evaluateCommand("ls -la /tmp", productionPolicy).action,
    "allow",
  );
});

test("subshell with redirection", () => {
  const tokens = tokenize("(echo hi) > file.txt");
  const cmds = extractCommands(tokens);
  assertTrue(cmds.some((c) => c.source === "subshell"));
});

// Double dash edge cases
test("double dash as word", () => {
  const tokens = tokenize("grep -- -v file");
  assertEquals(tokens.length, 4);
  assertEquals(tokens[1], { type: "word", value: "--" });
});

test("double dash command does not recurse for grep", () => {
  const tokens = tokenize("grep -- -v file");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "grep");
  assertEquals(cmds[0].fullText, "grep -- -v file");
});

test("double dash with sudo extraction", () => {
  const tokens = tokenize("sudo -- ls -la");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "sudo");
  assertEquals(cmds[1].name, "ls");
});

test("double dash in bash -c wrapper", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "grep", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  assertEquals(
    evaluateCommand("bash -c 'grep -- -v file'", policy).action,
    "allow",
  );
});

// Multiline string handling
test("multiline in double quotes", () => {
  const tokens = tokenize('echo "line1\nline2"');
  assertEquals(tokens.length, 2);
  assertTrue((tokens[1] as { value: string }).value.includes("\n"));
});

test("multiline in single quotes", () => {
  const tokens = tokenize("echo 'line1\nline2'");
  assertEquals(tokens.length, 2);
  assertTrue((tokens[1] as { value: string }).value.includes("\n"));
});

// Heredoc with shell script content
test("heredoc with shell commands - text content not executed", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "cat", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(
    evaluateCommand("cat <<EOF\nrm -rf /\nEOF", policy).action,
    "allow",
  );
});

test("bash heredoc - heredoc body not extracted", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "bash", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm", mode: "prefix" }],
  };
  assertEquals(
    evaluateCommand("bash <<EOF\nrm -rf /\nEOF", policy).action,
    "allow",
  );
});

// Complex stderr/stdout redirect combinations
test("redirect stdout to file stderr to stdout", () => {
  const tokens = tokenize("cmd > file 2>&1");
  const redirects = tokens.filter((t) => t.type === "redirect");
  assertEquals(redirects.length, 2);
  assertEquals((redirects[0] as { op: string }).op, ">");
  assertEquals((redirects[1] as { op: string }).op, "2>&");
});

test("redirect stderr first then stdout", () => {
  const tokens = tokenize("cmd 2>&1 > file");
  const redirects = tokens.filter((t) => t.type === "redirect");
  assertEquals(redirects.length, 2);
  assertEquals((redirects[0] as { op: string }).op, "2>&");
  assertEquals((redirects[1] as { op: string }).op, ">");
});

test("explicit fd redirects", () => {
  const tokens = tokenize("cmd 1>out.txt 2>err.txt");
  const redirects = tokens.filter((t) => t.type === "redirect");
  assertEquals(redirects.length, 2);
  assertEquals((redirects[0] as { op: string }).op, "1>");
  assertEquals((redirects[1] as { op: string }).op, "2>");
});

test("append with stderr merge", () => {
  const tokens = tokenize("cmd >> file 2>&1");
  const redirects = tokens.filter((t) => t.type === "redirect");
  assertEquals(redirects.length, 2);
  assertEquals((redirects[0] as { op: string }).op, ">>");
  assertEquals((redirects[1] as { op: string }).op, "2>&");
});

test("bash ampersand redirect syntax", () => {
  const tokens = tokenize("cmd &> file");
  const redirects = tokens.filter((t) => t.type === "redirect");
  assertTrue(redirects.length >= 1);
});

// Deny keyword in various contexts
test("deny substring in heredoc body - not extracted", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "bash", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(
    evaluateCommand("bash <<EOF\necho hi\nrm -rf /\nEOF", policy).action,
    "allow",
  );
});

test("deny keyword in quoted string still matches substring", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(evaluateCommand("echo 'rm -rf /'", policy).action, "deny");
});

// Complex multiline command
test("complex multiline command", () => {
  const tokens = tokenize(
    "echo 'start' && \\\n  echo 'middle' && \\\n  echo 'end'",
  );
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 3);
  assertEquals(
    cmds.every((c) => c.name === "echo"),
    true,
  );
});

// SECURITY: Double-dash wrapper bypass attempts
test("passthrough wrapper skips -- and extracts inner command", () => {
  const tokens = tokenize("time -- echo hi");
  const policy: PolicyCommands = {
    allow: [{ match: "time", mode: "prefix" }],
    ask: [],
    deny: [{ match: "echo", mode: "prefix" }],
  };
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "time");
  assertEquals(cmds[0].source, "direct");
  assertEquals(cmds[1].name, "echo");
  assertEquals(cmds[1].source, "wrapper-arg");
  const result = evaluateCommand("time -- echo hi", policy);
  assertEquals(result.action, "deny");
  assertTrue(result.reason?.includes("echo"));
});

test("bash -c -- 'cmd' treats -- as command string", () => {
  const tokens = tokenize("bash -c -- 'rm -rf /'");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "bash");
  assertEquals(cmds[1].name, "--");
});

test("bash -c without -- extracts correctly", () => {
  const tokens = tokenize("bash -c 'rm -rf /'");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "bash");
  assertEquals(cmds[1].name, "rm");
});

test("env -- VAR=val cmd extracts actual utility", () => {
  const tokens = tokenize("env -- VAR=val cmd");
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "env");
  assertEquals(cmds[1].name, "cmd");
});

// ==================== EXTENSIBILITY TESTS ====================
console.log("\n=== Extensibility Tests ===");

test("custom wrapper via config without code change", () => {
  const customRules = buildWrapperRuleMap([
    {
      name: "custom-wrapper",
      kind: "utility-operand",
    } as WrapperRuleConfig,
  ]);
  const tokens = tokenize("custom-wrapper -- cmd arg");
  const cmds = extractCommands(tokens, "direct", customRules);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "custom-wrapper");
  assertEquals(cmds[1].name, "cmd");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("evaluateCommand with custom wrapper rules", () => {
  const customRules = buildWrapperRuleMap([
    { name: "mycmd", kind: "utility-operand" } as WrapperRuleConfig,
  ]);
  const policy: PolicyCommands = {
    allow: [{ match: "inner", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  assertEquals(
    evaluateCommand("mycmd inner", policy, customRules).action,
    "allow",
  );
});

test("buildWrapperRuleMap replaces builtins when entries provided", () => {
  const rules = buildWrapperRuleMap([
    { name: "sudo", kind: "shell-c" } as WrapperRuleConfig,
  ]);
  const tokens = tokenize("sudo -c 'echo hi'");
  const cmds = extractCommands(tokens, "direct", rules);
  assertTrue(cmds.some((c) => c.name === "echo" && c.source === "wrapper-arg"));
});

test("buildWrapperRuleMap with undefined entries", () => {
  const rules = buildWrapperRuleMap(undefined);
  const tokens = tokenize("sudo ls");
  const cmds = extractCommands(tokens, "direct", rules);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "ls");
});

// ==================== END EDGE CASE & REGRESSION TESTS ====================

// ==================== NORMALIZE CONFIG TESTS ====================
console.log("\n=== Normalize Config Tests ===");

test("normalizeShellPolicyConfig - null input", () => {
  const config = normalizeShellPolicyConfig(null);
  assertEquals(config.commands, { allow: [], ask: [], deny: [] });
});

test("normalizeShellPolicyConfig - valid commands key", () => {
  const config = normalizeShellPolicyConfig({
    commands: {
      allow: [{ match: "ls", mode: "prefix" }],
      ask: [],
      deny: [],
    },
  });
  assertEquals(config.commands.allow.length, 1);
  assertEquals(config.commands.allow[0].match, "ls");
});

test("normalizeShellPolicyConfig - bare policy (no commands key)", () => {
  const config = normalizeShellPolicyConfig({
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [],
  });
  assertEquals(config.commands.allow.length, 1);
});

test("normalizeShellPolicyConfig - invalid wrappers filtered", () => {
  const config = normalizeShellPolicyConfig({
    commands: { allow: [], ask: [], deny: [] },
    wrappers: [
      { name: "valid", kind: "shell-c" },
      { name: "bad", kind: "nonexistent" },
      null,
      42,
    ],
  });
  assertEquals(config.wrappers?.length, 1);
  assertEquals(config.wrappers?.[0].name, "valid");
});

test("normalizeShellPolicyConfig - non-array fields default to empty", () => {
  const config = normalizeShellPolicyConfig({
    commands: { allow: "bad", ask: 42, deny: null },
  });
  assertEquals(config.commands, { allow: [], ask: [], deny: [] });
});

// ==================== END NORMALIZE CONFIG TESTS ====================

// ==================== EVALUATE REDIRECTS TESTS ====================

function makeCmd(
  redirects: { op: string; target: string }[],
): ExtractedCommand {
  return { name: "cmd", fullText: "cmd", redirects, source: "direct" };
}

test("evaluateRedirects - allow policy always allows", () => {
  const policy: RedirectPolicy = { action: "allow" };
  const cmds = [makeCmd([{ op: ">", target: "file.txt" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateRedirects - deny policy blocks unsafe output redirect", () => {
  const policy: RedirectPolicy = { action: "deny" };
  const cmds = [makeCmd([{ op: ">", target: "file.txt" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "deny");
});

test("evaluateRedirects - safe targets are allowed", () => {
  const policy: RedirectPolicy = {
    action: "deny",
    safeTargets: ["/dev/null", "/dev/stderr", "/dev/stdout"],
  };
  const cmds = [makeCmd([{ op: ">", target: "/dev/null" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateRedirects - unsafe target denied even with safeTargets set", () => {
  const policy: RedirectPolicy = {
    action: "deny",
    safeTargets: ["/dev/null"],
  };
  const cmds = [makeCmd([{ op: ">", target: "output.txt" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "deny");
});

test("evaluateRedirects - fd-dup allowed with allowFdDup", () => {
  const policy: RedirectPolicy = { action: "deny", allowFdDup: true };
  const cmds = [makeCmd([{ op: "2>&", target: "1" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateRedirects - fd-dup blocked without allowFdDup", () => {
  const policy: RedirectPolicy = { action: "deny", allowFdDup: false };
  const cmds = [makeCmd([{ op: "2>&", target: "1" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "deny");
});

test("evaluateRedirects - ask action returns ask", () => {
  const policy: RedirectPolicy = { action: "ask" };
  const cmds = [makeCmd([{ op: ">", target: "file.txt" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "ask");
});

test("evaluateRedirects - input redirect < always passes", () => {
  const policy: RedirectPolicy = { action: "deny" };
  const cmds = [makeCmd([{ op: "<", target: "input.txt" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateRedirects - heredoc << always passes", () => {
  const policy: RedirectPolicy = { action: "deny" };
  const cmds = [makeCmd([{ op: "<<", target: "EOF" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateRedirects - here-string <<< always passes", () => {
  const policy: RedirectPolicy = { action: "deny" };
  const cmds = [makeCmd([{ op: "<<<", target: "value" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateRedirects - fd-prefixed output redirect 2> evaluated", () => {
  const policy: RedirectPolicy = { action: "deny" };
  const cmds = [makeCmd([{ op: "2>", target: "err.log" }])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "deny");
});

test("evaluateRedirects - no redirects always allows", () => {
  const policy: RedirectPolicy = { action: "deny" };
  const cmds = [makeCmd([])];
  const result = evaluateRedirects(cmds, policy);
  assertEquals(result.action, "allow");
});

// ==================== END EVALUATE REDIRECTS TESTS ====================

// ==================== EVALUATE HEREDOCS TESTS ====================

test("evaluateHeredocs - allow policy always allows", () => {
  const policy: HeredocPolicy = { action: "allow" };
  const cmds = [makeCmd([{ op: "<<", target: "EOF" }])];
  const result = evaluateHeredocs(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateHeredocs - ask policy returns ask on heredoc", () => {
  const policy: HeredocPolicy = { action: "ask" };
  const cmds = [makeCmd([{ op: "<<", target: "EOF" }])];
  const result = evaluateHeredocs(cmds, policy);
  assertEquals(result.action, "ask");
});

test("evaluateHeredocs - deny policy returns deny on heredoc", () => {
  const policy: HeredocPolicy = { action: "deny" };
  const cmds = [makeCmd([{ op: "<<", target: "EOF" }])];
  const result = evaluateHeredocs(cmds, policy);
  assertEquals(result.action, "deny");
});

test("evaluateHeredocs - <<- also triggers", () => {
  const policy: HeredocPolicy = { action: "ask" };
  const cmds = [makeCmd([{ op: "<<-", target: "END" }])];
  const result = evaluateHeredocs(cmds, policy);
  assertEquals(result.action, "ask");
});

test("evaluateHeredocs - no heredocs always allows", () => {
  const policy: HeredocPolicy = { action: "ask" };
  const cmds = [makeCmd([{ op: ">", target: "file.txt" }])];
  const result = evaluateHeredocs(cmds, policy);
  assertEquals(result.action, "allow");
});

test("evaluateHeredocs - empty commands always allows", () => {
  const policy: HeredocPolicy = { action: "deny" };
  const result = evaluateHeredocs([], policy);
  assertEquals(result.action, "allow");
});

// ==================== END EVALUATE HEREDOCS TESTS ====================

// ==================== NORMALIZE UNIFIED POLICY TESTS ====================

test("normalizeUnifiedPolicyConfig - minimal config", () => {
  const config = normalizeUnifiedPolicyConfig({
    default: { commands: { allow: [], ask: [], deny: [] } },
  });
  assertEquals(config.default.commands, { allow: [], ask: [], deny: [] });
  assertEquals(config.modes, undefined);
});

test("normalizeUnifiedPolicyConfig - with modes", () => {
  const config = normalizeUnifiedPolicyConfig({
    default: { commands: { allow: [], ask: [], deny: [] } },
    modes: {
      plan: {
        tools: { edit: false, write: false },
        commands: { allow: [], ask: [], deny: [] },
      },
    },
  });
  assertTrue(config.modes?.plan !== undefined);
  assertEquals(config.modes?.plan.tools, { edit: false, write: false });
});

test("normalizeUnifiedPolicyConfig - with redirects and heredocs", () => {
  const config = normalizeUnifiedPolicyConfig({
    default: {
      commands: { allow: [], ask: [], deny: [] },
      redirects: { action: "allow" },
      heredocs: { action: "ask" },
    },
    modes: {
      plan: {
        commands: { allow: [], ask: [], deny: [] },
        redirects: {
          action: "deny",
          safeTargets: ["/dev/null"],
          allowFdDup: true,
        },
        heredocs: { action: "ask" },
      },
    },
  });
  assertEquals(config.default.redirects?.action, "allow");
  assertEquals(config.default.heredocs?.action, "ask");
  assertEquals(config.modes?.plan.redirects?.action, "deny");
  assertEquals(config.modes?.plan.redirects?.safeTargets, ["/dev/null"]);
  assertEquals(config.modes?.plan.redirects?.allowFdDup, true);
});

test("normalizeUnifiedPolicyConfig - null/undefined input defaults", () => {
  const config = normalizeUnifiedPolicyConfig(null);
  assertEquals(config.default.commands, { allow: [], ask: [], deny: [] });
  assertEquals(config.modes, undefined);
});

test("normalizeUnifiedPolicyConfig - missing fields default gracefully", () => {
  const config = normalizeUnifiedPolicyConfig({ default: {} });
  assertEquals(config.default.commands, { allow: [], ask: [], deny: [] });
  assertEquals(config.default.redirects, undefined);
  assertEquals(config.default.heredocs, undefined);
  assertEquals(config.default.tools, undefined);
});

// ==================== END NORMALIZE UNIFIED POLICY TESTS ====================

// Tests run inline above; print summary
console.log("\n=== Summary ===");
console.log(`${stats.passed} passed, ${stats.failed} failed`);
if (stats.failures.length > 0) {
  console.log("\nFailures:");
  stats.failures.forEach((f) => console.log(`  - ${f}`));
  process.exit(1);
}
