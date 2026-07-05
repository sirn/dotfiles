/**
 * Comprehensive Test Suite for Shell Policy Engine
 * Run with: nix run nixpkgs#tsx -- shell-policy.test.ts
 */

import {
  analyze,
  evaluate,
  tokenize,
  extractCommands,
  mergePoliciesStrict,
  buildWrapperRuleMap,
  normalizeShellPolicyConfig,
  normalizeUnifiedPolicyConfig,
  type PolicyCommands,
  type WrapperRuleConfig,
  type RedirectPolicy,
  type HeredocPolicy,
} from "../lib/shell-policy.ts";

// Wrapper rules used by tests that exercise wrapper extraction
const TEST_WRAPPER_RULE_CONFIGS: WrapperRuleConfig[] = [
  { name: "bash", kind: "shell-c" },
  { name: "sh", kind: "shell-c" },
  { name: "zsh", kind: "shell-c" },
  { name: "dash", kind: "shell-c" },
  { name: "ksh", kind: "shell-c" },
  { name: "sudo", kind: "utility-operand" },
  { name: "doas", kind: "utility-operand" },
  { name: "time", kind: "utility-operand" },
  { name: "nohup", kind: "utility-operand" },
  { name: "nice", kind: "utility-operand" },
  { name: "chroot", kind: "utility-operand" },
  { name: "timeout", kind: "utility-operand" },
  { name: "setsid", kind: "utility-operand" },
  { name: "env", kind: "env" },
  { name: "xargs", kind: "xargs" },
  { name: "docker", kind: "docker-run" },
  { name: "podman", kind: "docker-run" },
];
const TEST_WRAPPER_RULES = buildWrapperRuleMap(TEST_WRAPPER_RULE_CONFIGS);

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

// Leading comment line handling (fix for tokenizer breaking at comment start)
test("leading comment line with command", () => {
  const tokens = tokenize("# comment\nls -alh");
  const words = tokens.filter((t) => t.type === "word");
  assertEquals(words.length, 2);
  assertEquals(words[0], { type: "word", value: "ls" });
  assertEquals(words[1], { type: "word", value: "-alh" });
});

test("multiple leading comment lines", () => {
  const tokens = tokenize("# comment 1\n# comment 2\nls -alh");
  const words = tokens.filter((t) => t.type === "word");
  assertEquals(words.length, 2);
  assertEquals(words[0], { type: "word", value: "ls" });
  assertEquals(words[1], { type: "word", value: "-alh" });
});

test("comment-only input returns empty", () => {
  const tokens = tokenize("# just a comment");
  const words = tokens.filter((t) => t.type === "word");
  assertEquals(words.length, 0);
});

test("comment at line start with CRLF newline", () => {
  const tokens = tokenize("# comment\r\nls");
  const words = tokens.filter((t) => t.type === "word");
  assertEquals(words.length, 1);
  assertEquals(words[0], { type: "word", value: "ls" });
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

// Shell keywords are skipped
test("for loop keywords are skipped", () => {
  const tokens = tokenize("for repo in a b c; do echo $repo; done");
  const cmds = extractCommands(tokens);
  assertEquals(
    cmds.map((c) => c.name),
    ["echo"],
  );
});

test("while loop keywords are skipped", () => {
  const tokens = tokenize("while true; do echo hi; done");
  const cmds = extractCommands(tokens);
  assertEquals(
    cmds.map((c) => c.name),
    ["true", "echo"],
  );
});

test("if/then/else/fi keywords are skipped", () => {
  const tokens = tokenize("if test -f foo; then echo yes; else echo no; fi");
  const cmds = extractCommands(tokens);
  assertEquals(
    cmds.map((c) => c.name),
    ["test", "echo", "echo"],
  );
});

test("case/esac keywords are skipped", () => {
  const tokens = tokenize("case $x in; echo matched; esac");
  const cmds = extractCommands(tokens);
  assertEquals(
    cmds.map((c) => c.name),
    ["echo"],
  );
});

test("nested for with pipes", () => {
  const tokens = tokenize("for f in *.txt; do cat $f | grep hi; done");
  const cmds = extractCommands(tokens);
  assertEquals(
    cmds.map((c) => c.name),
    ["cat", "grep"],
  );
});

// Wrapper commands
test("bash -c wrapper", () => {
  const tokens = tokenize("bash -c 'echo hi'");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  const cmd = cmds.find((c) => c.name === "echo");
  assertTrue(cmd !== undefined);
  assertEquals(cmd?.source, "wrapper-arg");
});

test("sh -c wrapper", () => {
  const tokens = tokenize("sh -c 'ls -la'");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  const cmd = cmds.find((c) => c.name === "ls");
  assertTrue(cmd !== undefined);
  assertEquals(cmd?.source, "wrapper-arg");
});

test("sudo passthrough", () => {
  const tokens = tokenize("sudo rm -rf /");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "sudo");
  assertEquals(cmds[0].source, "direct");
  assertEquals(cmds[1].name, "rm");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("doas passthrough", () => {
  const tokens = tokenize("doas ls -la");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "ls");
});

test("xargs passthrough", () => {
  const tokens = tokenize("xargs rm");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "rm");
});

test("time passthrough without --", () => {
  const tokens = tokenize("time sleep 1");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "sleep");
});

test("time -- passthrough respects -- end-of-options", () => {
  const tokens = tokenize("time -- echo hi");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "echo");
});

test("env wrapper handles -- before command", () => {
  const tokens = tokenize("env -- VAR=val cmd arg");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cmd");
  assertEquals(cmds[1].fullText, "cmd arg");
});

test("env wrapper skips assignments", () => {
  const tokens = tokenize("env VAR=val echo hi");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "echo");
});

test("env with multiple assignments", () => {
  const tokens = tokenize("env A=1 B=2 C=3 cmd arg");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cmd");
  assertEquals(cmds[1].fullText, "cmd arg");
});

test("env skips assignments after double dash", () => {
  const tokens = tokenize("env -- KEY=val cmd");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cmd");
});

test("nohup wrapper extracts utility operand", () => {
  const tokens = tokenize("nohup cat file.txt");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cat");
});

test("sudo with only flags extracts no inner command", () => {
  const tokens = tokenize("sudo -h");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "sudo");
});

// Nested wrappers
test("nested wrappers bash -c with sudo", () => {
  const tokens = tokenize("sudo bash -c 'rm -rf /'");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 3);
  assertTrue(cmds.some((c) => c.name === "rm"));
});

test("deeply nested", () => {
  const tokens = tokenize("echo $(bash -c 'ls $(pwd)')");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
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
  assertEquals(evaluate("ls", { commands: policy }).action, "deny");
});

test("exact match - case insensitive", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "LS", mode: "exact" }],
  };
  assertEquals(evaluate("ls", { commands: policy }).action, "deny");
});

test("exact match - whitespace trimmed", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "ls", mode: "exact" }],
  };
  assertEquals(evaluate("  ls  ", { commands: policy }).action, "deny");
});

test("exact match - args don't match", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "ls", mode: "exact" }],
  };
  assertEquals(evaluate("ls -la", { commands: policy }).action, "default");
});

// Prefix match mode
test("prefix match - matches", () => {
  assertEquals(evaluate("ls -la", { commands: samplePolicy }).action, "allow");
});

test("prefix match - exact also matches", () => {
  assertEquals(evaluate("ls", { commands: samplePolicy }).action, "allow");
});

test("prefix match - case insensitive", () => {
  assertEquals(evaluate("LS -LA", { commands: samplePolicy }).action, "allow");
});

test("prefix match - no match when pattern longer", () => {
  assertEquals(evaluate("l", { commands: samplePolicy }).action, "default");
});

// Word-boundary-aware prefix tests
test("prefix match - word boundary: no match on longer token", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  assertEquals(evaluate("lsof", { commands: policy }).action, "default");
});

test("prefix match - word boundary: space-separated args match", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  assertEquals(evaluate("ls -la", { commands: policy }).action, "allow");
});

test("prefix match - word boundary: find / blocks root scan", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "find", mode: "prefix" }],
    ask: [],
    deny: [{ match: "find /", mode: "prefix" }],
  };
  assertEquals(evaluate("find /", { commands: policy }).action, "deny");
  assertEquals(
    evaluate("find / -name foo", { commands: policy }).action,
    "deny",
  );
});

test("prefix match - word boundary: find / does not block find /etc", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "find", mode: "prefix" }],
    ask: [],
    deny: [{ match: "find /", mode: "prefix" }],
  };
  assertEquals(
    evaluate("find /etc/hosts", { commands: policy }).action,
    "allow",
  );
});

test("prefix match - word boundary: find /nix does not match find /nix/store", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "find", mode: "prefix" }],
    ask: [],
    deny: [{ match: "find /nix", mode: "prefix" }],
  };
  assertEquals(
    evaluate("find /nix/store", { commands: policy }).action,
    "allow",
  );
  assertEquals(
    evaluate("find /nix -type f", { commands: policy }).action,
    "deny",
  );
});

test("prefix match - word boundary: find /nix/store matches exact path", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "find", mode: "prefix" }],
    ask: [],
    deny: [{ match: "find /nix/store", mode: "prefix" }],
  };
  assertEquals(
    evaluate("find /nix/store", { commands: policy }).action,
    "deny",
  );
  assertEquals(
    evaluate("find /nix/store -name foo", { commands: policy }).action,
    "deny",
  );
});

test("prefix match - word boundary: multi-word last part is one token", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "cat", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  assertEquals(evaluate("catalog", { commands: policy }).action, "default");
  assertEquals(evaluate("cat file.txt", { commands: policy }).action, "allow");
});

// Substring match mode
test("substring match - exact", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(evaluate("rm -rf /", { commands: policy }).action, "deny");
});

test("substring match - in pipeline", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(
    evaluate("echo hi && rm -rf /", { commands: policy }).action,
    "deny",
  );
});

test("substring match - no partial word match", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf", mode: "substring" }],
  };
  assertEquals(
    evaluate("grm -rf file", { commands: policy }).action,
    "default",
  );
});

test("substring match - case insensitive", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "RM -RF /", mode: "substring" }],
  };
  assertEquals(
    evaluate("echo hi && rm -rf /", { commands: policy }).action,
    "deny",
  );
});

test("substring match - special characters", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "git push", mode: "substring" }],
  };
  assertEquals(
    evaluate("git push origin main", { commands: policy }).action,
    "deny",
  );
});

test("token substring - exact token match", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm", mode: "substring" }],
  };
  assertEquals(evaluate("rm -rf /", { commands: policy }).action, "deny");
});

test("token substring - no partial token match", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm", mode: "substring" }],
  };
  assertEquals(
    evaluate("firmware update", { commands: policy }).action,
    "default",
  );
});

test("token substring - multi-token position-independent", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "-X POST", mode: "substring" }],
  };
  assertEquals(
    evaluate("curl --foo -X POST https://example.com", { commands: policy })
      .action,
    "deny",
  );
});

test("token substring - multi-token at start", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "-X POST", mode: "substring" }],
  };
  assertEquals(evaluate("-X POST", { commands: policy }).action, "deny");
});

test("token substring - multi-token no match when tokens differ", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "-X POST", mode: "substring" }],
  };
  assertEquals(
    evaluate("curl -XPOST https://example.com", { commands: policy }).action,
    "default",
  );
});

test("token substring - case insensitive tokens", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "-x post", mode: "substring" }],
  };
  assertEquals(
    evaluate("curl -X POST url", { commands: policy }).action,
    "deny",
  );
});

test("token substring - deny overrides allow for curl mutation", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "curl", mode: "prefix" }],
    ask: [],
    deny: [{ match: "-X POST", mode: "substring" }],
  };
  assertEquals(
    evaluate("curl -s -X POST https://api.example.com", { commands: policy })
      .action,
    "deny",
  );
  assertEquals(
    evaluate("curl -s https://api.example.com", { commands: policy }).action,
    "allow",
  );
});

// Priority tests
test("deny takes priority over allow", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "ls", mode: "prefix" }],
    ask: [],
    deny: [{ match: "ls", mode: "prefix" }],
  };
  assertEquals(evaluate("ls -la", { commands: policy }).action, "deny");
});

test("deny takes priority over ask", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "sudo", mode: "prefix" }],
    deny: [{ match: "sudo", mode: "prefix" }],
  };
  assertEquals(evaluate("sudo ls", { commands: policy }).action, "deny");
});

test("ask escalates over allow", () => {
  assertEquals(
    evaluate("echo hi && rm file", { commands: samplePolicy }).action,
    "ask",
  );
});

test("allow downgrades to default when later command unmatched", () => {
  assertEquals(
    evaluate("echo hi && unknown_cmd", { commands: samplePolicy }).action,
    "default",
  );
});

test("ask not downgraded when later command unmatched", () => {
  assertEquals(
    evaluate("rm file && unknown_cmd", { commands: samplePolicy }).action,
    "ask",
  );
});

test("allow-unmatched-allow does not re-grant allow", () => {
  assertEquals(
    evaluate("echo hi | unknown_cmd | echo bye", { commands: samplePolicy })
      .action,
    "default",
  );
});

test("allow-allow stays allow", () => {
  assertEquals(
    evaluate("echo hi | echo bye", { commands: samplePolicy }).action,
    "allow",
  );
});

test("unmatched-allow stays default", () => {
  assertEquals(
    evaluate("unknown_cmd | echo hi", { commands: samplePolicy }).action,
    "default",
  );
});

test("multiple commands - one deny triggers deny", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "echo", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(
    evaluate("echo hi && rm -rf /", { commands: policy }).action,
    "deny",
  );
});

// Wrapper command extraction
test("wrapper command extraction - sudo denied", () => {
  assertEquals(
    evaluate("sudo ls -la", {
      commands: samplePolicy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});

test("wrapper command extraction - sudo prefix match", () => {
  assertEquals(
    evaluate("sudo ls", {
      commands: samplePolicy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});

test("wrapper command extraction - bash -c wrapper", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "echo", mode: "prefix" }],
    ask: [],
    deny: [{ match: "rm", mode: "prefix" }],
  };
  assertEquals(
    evaluate("bash -c 'rm -rf /'", {
      commands: policy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});

test("wrapper command extraction - nested wrappers", () => {
  assertEquals(
    evaluate("sudo bash -c 'ls -la'", {
      commands: samplePolicy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});

// Error handling
test("parse error returns ask", () => {
  const result = evaluate("echo 'unclosed", { commands: samplePolicy });
  assertEquals(result.action, "ask");
  assertTrue(result.reason?.includes("Unparseable"));
});

test("empty command returns ask", () => {
  assertEquals(evaluate("", { commands: samplePolicy }).action, "ask");
});

test("whitespace-only command returns ask", () => {
  assertEquals(evaluate("   \n\t  ", { commands: samplePolicy }).action, "ask");
});

test("unknown command returns default", () => {
  assertEquals(
    evaluate("unknown_cmd", { commands: samplePolicy }).action,
    "default",
  );
});

test("unknown match mode treated as no-match", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "ls", mode: "nonexistent" as any }],
    ask: [],
    deny: [],
  };
  assertEquals(evaluate("ls", { commands: policy }).action, "default");
});

test("bare variable as command returns ask", () => {
  assertEquals(evaluate("$CMD", { commands: samplePolicy }).action, "ask");
});

test("${VAR} as command returns ask", () => {
  assertEquals(evaluate("${CMD}", { commands: samplePolicy }).action, "ask");
});

// Args match mode
test("args match - wildcard program, required arg present", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:-X POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl -X POST https://example.com", { commands: policy }).action,
    "ask",
  );
});

test("args match - wildcard program, arg anywhere in command", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:-X POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate(
      'curl -H "Content-Type: application/json" -X POST https://example.com',
      { commands: policy },
    ).action,
    "ask",
  );
});

test("args match - wildcard program, required arg absent", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:-X POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl https://example.com", { commands: policy }).action,
    "default",
  );
});

test("args match - wildcard program, different flag value", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:-X POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl -X GET https://example.com", { commands: policy }).action,
    "default",
  );
});

test("args match - program prefix matches", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "gh api:-f", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("gh api --paginate -f key=val /repos", { commands: policy })
      .action,
    "ask",
  );
});

test("args match - program prefix mismatch", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "gh api:-f", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("gh foo bar -f x", { commands: policy }).action,
    "default",
  );
});

test("args match - required arg absent for prefix pattern", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "gh api:-f", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("gh api /repos", { commands: policy }).action,
    "default",
  );
});

test("args match - multiple required args, all present", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:--request POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --request POST https://example.com", { commands: policy })
      .action,
    "ask",
  );
});

test("args match - multiple required args, one missing", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:--request POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --request GET https://example.com", { commands: policy })
      .action,
    "default",
  );
});

test("args match - case insensitive program prefix", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "GH API:-f", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("gh api -f key=val", { commands: policy }).action,
    "ask",
  );
});

test("args match - case insensitive required arg", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "*:-X POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl -x post https://example.com", { commands: policy }).action,
    "ask",
  );
});

test("args match - no colon is pure prefix match", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "gh api", mode: "args" }],
    ask: [],
    deny: [],
  };
  assertEquals(evaluate("gh api /repos", { commands: policy }).action, "allow");
  assertEquals(evaluate("gh foo", { commands: policy }).action, "default");
});

test("args match - allow curl read, ask on curl mutation", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "curl", mode: "prefix" }],
    ask: [{ match: "*:-X POST", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl https://example.com", { commands: policy }).action,
    "allow",
  );
  assertEquals(
    evaluate("curl -X POST https://example.com", { commands: policy }).action,
    "ask",
  );
});

test("args match - prefix words excluded from arg search", () => {
  // 'api' is the 2nd word of prefix 'gh api'; without -f in the trailing
  // args, the rule must not fire
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "gh api:-f", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("gh api /repos", { commands: policy }).action,
    "default",
  );
});

// Args match - implicit curl mutation flags
test("args match - curl -d implicitly POSTs", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "curl", mode: "prefix" }],
    ask: [{ match: "curl:-d", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate('curl -d \'{"key":"val"}\' https://example.com', {
      commands: policy,
    }).action,
    "ask",
  );
  assertEquals(
    evaluate("curl https://example.com", { commands: policy }).action,
    "allow",
  );
});

test("args match - curl --data implicitly POSTs", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--data", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --data 'payload' https://example.com", { commands: policy })
      .action,
    "ask",
  );
});

test("args match - curl --data-binary", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--data-binary", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --data-binary @file.bin https://example.com", {
      commands: policy,
    }).action,
    "ask",
  );
});

test("args match - curl --data-raw", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--data-raw", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --data-raw 'data' https://example.com", { commands: policy })
      .action,
    "ask",
  );
});

test("args match - curl --data-urlencode", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--data-urlencode", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --data-urlencode 'key=val' https://example.com", {
      commands: policy,
    }).action,
    "ask",
  );
});

test("args match - curl -F implicitly POSTs", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "curl", mode: "prefix" }],
    ask: [{ match: "curl:-F", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl -F file=@photo.jpg https://example.com/upload", {
      commands: policy,
    }).action,
    "ask",
  );
  assertEquals(
    evaluate("curl https://example.com", { commands: policy }).action,
    "allow",
  );
});

test("args match - curl --form", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--form", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --form file=@photo.jpg https://example.com", {
      commands: policy,
    }).action,
    "ask",
  );
});

test("args match - curl --form-string", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--form-string", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --form-string name=value https://example.com", {
      commands: policy,
    }).action,
    "ask",
  );
});

test("args match - curl -T implicitly PUTs", () => {
  const policy: PolicyCommands = {
    allow: [{ match: "curl", mode: "prefix" }],
    ask: [{ match: "curl:-T", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl -T file.tar.gz https://example.com/upload", {
      commands: policy,
    }).action,
    "ask",
  );
  assertEquals(
    evaluate("curl https://example.com", { commands: policy }).action,
    "allow",
  );
});

test("args match - curl --upload-file implicitly PUTs", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:--upload-file", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("curl --upload-file file.bin https://example.com", {
      commands: policy,
    }).action,
    "ask",
  );
});

test("args match - curl prefix rejects non-curl commands", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [{ match: "curl:-d", mode: "args" }],
    deny: [],
  };
  assertEquals(
    evaluate("some-tool -d payload https://example.com", { commands: policy })
      .action,
    "default",
  );
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
  const merged = mergePoliciesStrict(p1, p2);
  assertEquals(merged.allow.length, 1);
  assertEquals(merged.ask.length, 1);
  assertEquals(merged.deny.length, 1);
  assertEquals(merged.allow[0].match, "ls");
  assertEquals(merged.ask[0].match, "rm");
  assertEquals(merged.deny[0].match, "sudo");
});

test("combines multiple policies", () => {
  const merged = mergePoliciesStrict(
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
  const merged = mergePoliciesStrict(p1, p2);
  assertEquals(merged.allow.length, 1);
  assertEquals(merged.ask.length, 0);
  assertEquals(merged.deny.length, 0);
});

test("both empty", () => {
  const p1: PolicyCommands = { allow: [], ask: [], deny: [] };
  const p2: PolicyCommands = { allow: [], ask: [], deny: [] };
  const merged = mergePoliciesStrict(p1, p2);
  assertEquals(merged.allow.length, 0);
  assertEquals(merged.ask.length, 0);
  assertEquals(merged.deny.length, 0);
});

test("order preservation", () => {
  const merged = mergePoliciesStrict(
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
    { match: "jj commit", mode: "prefix" },
    { match: "jj describe", mode: "prefix" },
    { match: "jj new", mode: "prefix" },
  ],
  ask: [
    { match: "chmod", mode: "substring" },
    { match: "chown", mode: "substring" },
    { match: "rm", mode: "substring" },
    { match: "docker exec", mode: "substring" },
    { match: "nix run", mode: "substring" },
    { match: "jj edit", mode: "substring" },
    { match: "jj squash", mode: "substring" },
  ],
  deny: [
    { match: "sudo", mode: "prefix" },
    { match: "doas", mode: "prefix" },
    { match: "git push", mode: "substring" },
    { match: "rm -rf /", mode: "substring" },
    { match: "--method POST", mode: "substring" },
    { match: "--method PUT", mode: "substring" },
    { match: "--method DELETE", mode: "substring" },
  ],
};

// Production policy tests
test("git push denied", () => {
  assertEquals(
    evaluate("git push origin main", { commands: productionPolicy }).action,
    "deny",
  );
});
test("git status allowed", () => {
  assertEquals(
    evaluate("git status", { commands: productionPolicy }).action,
    "default",
  );
});
test("rm -rf / denied", () => {
  assertEquals(
    evaluate("rm -rf /", { commands: productionPolicy }).action,
    "deny",
  );
});
test("rm -rf / in text denied", () => {
  assertEquals(
    evaluate("echo hi && rm -rf /", { commands: productionPolicy }).action,
    "deny",
  );
});
test("chmod asks", () => {
  assertEquals(
    evaluate("chmod +x script.sh", { commands: productionPolicy }).action,
    "ask",
  );
});
test("rm asks", () => {
  assertEquals(
    evaluate("rm file.txt", { commands: productionPolicy }).action,
    "ask",
  );
});
test("docker exec asks", () => {
  assertEquals(
    evaluate("docker exec -it container bash", { commands: productionPolicy })
      .action,
    "ask",
  );
});
test("nix run asks", () => {
  assertEquals(
    evaluate("nix run nixpkgs#something", { commands: productionPolicy })
      .action,
    "ask",
  );
});
test("jj describe allowed", () => {
  assertEquals(
    evaluate("jj describe -m 'update'", { commands: productionPolicy }).action,
    "allow",
  );
});
test("jj commit allowed", () => {
  assertEquals(
    evaluate("jj commit -m 'update'", { commands: productionPolicy }).action,
    "allow",
  );
});
test("jj new allowed", () => {
  assertEquals(
    evaluate("jj new -m 'next'", { commands: productionPolicy }).action,
    "allow",
  );
});
test("jj edit asks", () => {
  assertEquals(
    evaluate("jj edit abc123", { commands: productionPolicy }).action,
    "ask",
  );
});
test("jj squash asks", () => {
  assertEquals(
    evaluate("jj squash", { commands: productionPolicy }).action,
    "ask",
  );
});
test("gh api --method POST denied", () => {
  assertEquals(
    evaluate("gh api repos/foo --method POST", { commands: productionPolicy })
      .action,
    "deny",
  );
});
test("gh api GET allowed", () => {
  assertEquals(
    evaluate("gh api repos/foo", { commands: productionPolicy }).action,
    "default",
  );
});
test("sudo denied", () => {
  assertEquals(
    evaluate("sudo ls -la", { commands: productionPolicy }).action,
    "deny",
  );
});
test("quoted > not redirect", () => {
  assertEquals(
    evaluate("jaq '.x > .y' file.json", { commands: productionPolicy }).action,
    "default",
  );
});
test("echo with quoted > allowed", () => {
  assertEquals(
    evaluate("echo 'hello > world'", { commands: productionPolicy }).action,
    "allow",
  );
});
test("grep with quoted > allowed", () => {
  assertEquals(
    evaluate("grep '>' file.txt", { commands: productionPolicy }).action,
    "allow",
  );
});

// Regression tests
test("bash -c git push denied", () => {
  assertEquals(
    evaluate("bash -c 'git push'", {
      commands: productionPolicy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});
test("echo with quoted rm -rf / - quoted content is single token", () => {
  assertEquals(
    evaluate("echo 'rm -rf /'", { commands: productionPolicy }).action,
    "allow",
  );
});
test("grep with sudo pattern allowed", () => {
  assertEquals(
    evaluate("grep 'sudo' file.txt", { commands: productionPolicy }).action,
    "allow",
  );
});
test("bash -c rm -rf / denied", () => {
  assertEquals(
    evaluate("bash -c 'rm -rf /'", {
      commands: productionPolicy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});
test("sudo in substitution denied", () => {
  assertEquals(
    evaluate("$(sudo reboot)", { commands: productionPolicy }).action,
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
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
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
    evaluate("ls -la /tmp", { commands: productionPolicy }).action,
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
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
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
    evaluate("bash -c 'grep -- -v file'", {
      commands: policy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
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
    evaluate("cat <<EOF\nrm -rf /\nEOF", { commands: policy }).action,
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
    evaluate("bash <<EOF\nrm -rf /\nEOF", { commands: policy }).action,
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
    evaluate("bash <<EOF\necho hi\nrm -rf /\nEOF", { commands: policy }).action,
    "allow",
  );
});

test("quoted string content not matched by token substring", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm -rf /", mode: "substring" }],
  };
  assertEquals(
    evaluate("echo 'rm -rf /'", { commands: policy }).action,
    "default",
  );
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
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "time");
  assertEquals(cmds[0].source, "direct");
  assertEquals(cmds[1].name, "echo");
  assertEquals(cmds[1].source, "wrapper-arg");
  const result = evaluate("time -- echo hi", {
    commands: policy,
    wrappers: TEST_WRAPPER_RULE_CONFIGS,
  });
  assertEquals(result.action, "deny");
  assertTrue(result.reason?.includes("echo"));
});

test("bash -c -- 'cmd' treats -- as command string", () => {
  const tokens = tokenize("bash -c -- 'rm -rf /'");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "bash");
  assertEquals(cmds[1].name, "--");
});

test("bash -c without -- extracts correctly", () => {
  const tokens = tokenize("bash -c 'rm -rf /'");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "bash");
  assertEquals(cmds[1].name, "rm");
});

test("env -- VAR=val cmd extracts actual utility", () => {
  const tokens = tokenize("env -- VAR=val cmd");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
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
  const customRules: WrapperRuleConfig[] = [
    { name: "mycmd", kind: "utility-operand" },
  ];
  const policy: PolicyCommands = {
    allow: [{ match: "inner", mode: "prefix" }],
    ask: [],
    deny: [],
  };
  assertEquals(
    evaluate("mycmd inner", { commands: policy, wrappers: customRules }).action,
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

test("buildWrapperRuleMap with undefined entries returns empty map", () => {
  const rules = buildWrapperRuleMap(undefined);
  const tokens = tokenize("sudo ls");
  const cmds = extractCommands(tokens, "direct", rules);
  // No wrappers configured: only the direct sudo command, no inner extraction
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "sudo");
});

// ==================== END EDGE CASE & REGRESSION TESTS ====================

// ==================== DOCKER-RUN WRAPPER TESTS ====================
console.log("\n=== Docker-Run Wrapper Tests ===");

test("docker run basic extracts inner command", () => {
  const tokens = tokenize("docker run ubuntu ls -la");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[0].name, "docker");
  assertEquals(cmds[1].name, "ls");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("docker run with flags extracts inner command", () => {
  const tokens = tokenize("docker run --rm -it -v /a:/b -e FOO=bar ubuntu ls");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "ls");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("docker run with -- extracts inner command", () => {
  const tokens = tokenize("docker run --rm -- ubuntu ls -la");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "ls");
  assertEquals(cmds[1].fullText, "ls -la");
});

test("docker exec extracts inner command", () => {
  const tokens = tokenize("docker exec -it mycontainer bash");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "bash");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("podman run extracts inner command", () => {
  const tokens = tokenize("podman run alpine cat /etc/hosts");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 2);
  assertEquals(cmds[1].name, "cat");
  assertEquals(cmds[1].source, "wrapper-arg");
});

test("docker run with no inner command returns only docker", () => {
  const tokens = tokenize("docker run ubuntu");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "docker");
});

test("docker build is not a wrapper subcommand", () => {
  const tokens = tokenize("docker build .");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "docker");
});

test("docker run nested wrappers extracts deeply", () => {
  const tokens = tokenize("docker run ubuntu sudo ls");
  const cmds = extractCommands(tokens, "direct", TEST_WRAPPER_RULES);
  // docker (direct), sudo (wrapper-arg), ls (wrapper-arg)
  assertEquals(cmds.length, 3);
  assertTrue(cmds.some((c) => c.name === "sudo"));
  assertTrue(cmds.some((c) => c.name === "ls"));
});

test("evaluateCommand docker run rm -rf / denied", () => {
  const policy: PolicyCommands = {
    allow: [],
    ask: [],
    deny: [{ match: "rm", mode: "prefix" }],
  };
  assertEquals(
    evaluate("docker run ubuntu rm -rf /", {
      commands: policy,
      wrappers: TEST_WRAPPER_RULE_CONFIGS,
    }).action,
    "deny",
  );
});

test("extractCommands with empty wrapper rules does not extract sudo", () => {
  const emptyRules = buildWrapperRuleMap([]);
  const tokens = tokenize("sudo ls");
  const cmds = extractCommands(tokens, "direct", emptyRules);
  assertEquals(cmds.length, 1);
  assertEquals(cmds[0].name, "sudo");
});

// ==================== END DOCKER-RUN WRAPPER TESTS ====================

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

function evalRedirects(
  cmd: string,
  policy: RedirectPolicy,
): { action: string } {
  const result = analyze(cmd, { redirects: policy });
  return { action: result.phases.redirects?.action ?? "allow" };
}

test("evaluateRedirects - allow policy always allows", () => {
  const policy: RedirectPolicy = { action: "allow" };
  assertEquals(evalRedirects("cmd > file.txt", policy).action, "allow");
});

test("evaluateRedirects - deny policy blocks unsafe output redirect", () => {
  const policy: RedirectPolicy = { action: "deny" };
  assertEquals(evalRedirects("cmd > file.txt", policy).action, "deny");
});

test("evaluateRedirects - safe targets are allowed", () => {
  const policy: RedirectPolicy = {
    action: "deny",
    safeTargets: ["/dev/null", "/dev/stderr", "/dev/stdout"],
  };
  assertEquals(evalRedirects("cmd > /dev/null", policy).action, "allow");
});

test("evaluateRedirects - unsafe target denied even with safeTargets set", () => {
  const policy: RedirectPolicy = {
    action: "deny",
    safeTargets: ["/dev/null"],
  };
  assertEquals(evalRedirects("cmd > output.txt", policy).action, "deny");
});

test("evaluateRedirects - fd-dup allowed with allowFdDup", () => {
  const policy: RedirectPolicy = { action: "deny", allowFdDup: true };
  assertEquals(evalRedirects("cmd 2>&1", policy).action, "allow");
});

test("evaluateRedirects - fd-dup blocked without allowFdDup", () => {
  const policy: RedirectPolicy = { action: "deny", allowFdDup: false };
  assertEquals(evalRedirects("cmd 2>&1", policy).action, "deny");
});

test("evaluateRedirects - ask action returns ask", () => {
  const policy: RedirectPolicy = { action: "ask" };
  assertEquals(evalRedirects("cmd > file.txt", policy).action, "ask");
});

test("evaluateRedirects - input redirect < always passes", () => {
  const policy: RedirectPolicy = { action: "deny" };
  assertEquals(evalRedirects("cmd < input.txt", policy).action, "allow");
});

test("evaluateRedirects - heredoc << always passes", () => {
  const policy: RedirectPolicy = { action: "deny" };
  assertEquals(evalRedirects("cmd << EOF\nEOF", policy).action, "allow");
});

test("evaluateRedirects - here-string <<< always passes", () => {
  const policy: RedirectPolicy = { action: "deny" };
  assertEquals(evalRedirects("cmd <<< value", policy).action, "allow");
});

test("evaluateRedirects - fd-prefixed output redirect 2> evaluated", () => {
  const policy: RedirectPolicy = { action: "deny" };
  assertEquals(evalRedirects("cmd 2> err.log", policy).action, "deny");
});

test("evaluateRedirects - no redirects always allows", () => {
  const policy: RedirectPolicy = { action: "deny" };
  assertEquals(evalRedirects("cmd", policy).action, "allow");
});

// ==================== END EVALUATE REDIRECTS TESTS ====================

// ==================== EVALUATE HEREDOCS TESTS ====================

function evalHeredocs(cmd: string, policy: HeredocPolicy): { action: string } {
  const result = analyze(cmd, { heredocs: policy });
  return { action: result.phases.heredocs?.action ?? "allow" };
}

test("evaluateHeredocs - allow policy always allows", () => {
  const policy: HeredocPolicy = { action: "allow" };
  assertEquals(evalHeredocs("cmd << EOF\nEOF", policy).action, "allow");
});

test("evaluateHeredocs - ask policy returns ask on heredoc", () => {
  const policy: HeredocPolicy = { action: "ask" };
  assertEquals(evalHeredocs("cmd << EOF\nEOF", policy).action, "ask");
});

test("evaluateHeredocs - deny policy returns deny on heredoc", () => {
  const policy: HeredocPolicy = { action: "deny" };
  assertEquals(evalHeredocs("cmd << EOF\nEOF", policy).action, "deny");
});

test("evaluateHeredocs - <<- also triggers", () => {
  const policy: HeredocPolicy = { action: "ask" };
  assertEquals(evalHeredocs("cmd <<- END\nEND", policy).action, "ask");
});

test("evaluateHeredocs - no heredocs always allows", () => {
  const policy: HeredocPolicy = { action: "ask" };
  assertEquals(evalHeredocs("cmd > file.txt", policy).action, "allow");
});

test("evaluateHeredocs - empty commands always allows", () => {
  const policy: HeredocPolicy = { action: "deny" };
  assertEquals(evalHeredocs("", policy).action, "allow");
});

// ==================== END EVALUATE HEREDOCS TESTS ====================

// ==================== UNIFIED EVALUATION TESTS ====================

test("evaluate - neutral redirects and heredocs do not promote default", () => {
  const policy = {
    commands: {
      allow: [{ match: "ls", mode: "prefix" as const }],
      ask: [],
      deny: [],
    },
    redirects: { action: "deny" as const },
    heredocs: { action: "ask" as const },
  };

  const analysis = analyze("unknown_cmd", policy);
  const result = evaluate("unknown_cmd", policy);

  assertEquals(analysis.final.action, "default");
  assertEquals(analysis.final.decidedBy, "default");
  assertEquals(analysis.phases.redirects?.reason, "No unsafe redirects");
  assertEquals(analysis.phases.redirects?.triggered, false);
  assertEquals(analysis.phases.heredocs?.reason, "No heredocs");
  assertEquals(analysis.phases.heredocs?.triggered, false);
  assertEquals(result.action, "default");
});

test("evaluate - allow redirect policy does not promote default commands", () => {
  const result = evaluate("unknown_cmd > file.txt", {
    commands: { allow: [], ask: [], deny: [] },
    redirects: { action: "allow" },
  });
  assertEquals(result.action, "default");
  assertEquals(result.decidedBy, "default");
});

test("evaluate - allow heredoc policy does not promote default commands", () => {
  const result = evaluate("unknown_cmd << EOF\nEOF", {
    commands: { allow: [], ask: [], deny: [] },
    heredocs: { action: "allow" },
  });
  assertEquals(result.action, "default");
  assertEquals(result.decidedBy, "default");
});

test("evaluate - deny beats ask and allow across categories", () => {
  const result = evaluate("cat > out.txt <<EOF\nhello\nEOF", {
    commands: {
      allow: [{ match: "cat", mode: "exact" }],
      ask: [],
      deny: [],
    },
    redirects: { action: "deny" },
    heredocs: { action: "ask" },
  });

  assertEquals(result.action, "deny");
  assertEquals(result.decidedBy, "redirects");
});

test("analyze - tie-breaking is deterministic across phases", () => {
  const commandWins = analyze("cat <<EOF\nhello\nEOF", {
    commands: {
      allow: [],
      ask: [{ match: "cat", mode: "exact" }],
      deny: [],
    },
    heredocs: { action: "ask" },
  });
  assertEquals(commandWins.final.action, "ask");
  assertEquals(commandWins.final.decidedBy, "commands");

  const redirectWins = analyze("unknown_cmd > out.txt <<EOF\nhello\nEOF", {
    commands: { allow: [], ask: [], deny: [] },
    redirects: { action: "ask" },
    heredocs: { action: "ask" },
  });
  assertEquals(redirectWins.final.action, "ask");
  assertEquals(redirectWins.final.decidedBy, "redirects");
});

test("analyze - exposes ordered command match payloads for CLI", () => {
  const analysis = analyze("ls -la", {
    commands: {
      allow: [{ match: "ls", mode: "prefix" }],
      ask: [{ match: "-la", mode: "substring" }],
      deny: [{ match: "ls -la", mode: "exact" }],
    },
  });

  assertEquals(analysis.commands[0].action, "deny");
  assertEquals(
    analysis.commands[0].matches.map(
      (match) => `${match.category}:${match.entry.match}`,
    ),
    ["deny:ls -la", "ask:-la", "allow:ls"],
  );
  assertEquals(analysis.final.match, {
    category: "deny",
    entry: { match: "ls -la", mode: "exact" },
  });
});

test("analyze - exposes phase summaries used by CLI and runtime", () => {
  const analysis = analyze("echo hi > output.txt", {
    commands: {
      allow: [{ match: "echo", mode: "prefix" }],
      ask: [],
      deny: [],
    },
    redirects: { action: "ask" },
  });

  assertEquals(analysis.final.action, "ask");
  assertEquals(analysis.final.decidedBy, "redirects");
  assertEquals(analysis.phases.redirects, {
    action: "ask",
    reason: 'Redirect to "output.txt"',
    triggered: true,
    redirects: [
      {
        cmdName: "echo",
        op: ">",
        target: "output.txt",
        action: "ask",
        reason: 'Redirect to "output.txt"',
      },
    ],
  });
});

// ==================== END UNIFIED EVALUATION TESTS ====================

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

// ==================== MERGE EVALUATION POLICIES TESTS ====================
console.log("\n=== Merge Evaluation Policies Tests ===");

import {
  mergeEvaluationPolicies,
  type ModePolicy,
} from "../lib/shell-policy.ts";

test("mergeEvaluationPolicies - no mode returns default as-is", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [] },
    redirects: { action: "allow" },
    heredocs: { action: "ask" },
    wrappers: [{ name: "bash", kind: "shell-c" }],
  };
  const result = mergeEvaluationPolicies(defaultPolicy, undefined);
  assertEquals(result.commands, defaultPolicy.commands);
  assertEquals(result.redirects, defaultPolicy.redirects);
  assertEquals(result.heredocs, defaultPolicy.heredocs);
  assertEquals(result.wrappers, defaultPolicy.wrappers);
});

test("mergeEvaluationPolicies - mode commands concatenated with default", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [] },
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [{ match: "cp", mode: "prefix" }] },
  };
  const result = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  assertEquals(result.commands!.allow.length, 1);
  assertEquals(result.commands!.deny.length, 1);
  assertEquals(result.commands!.allow[0].match, "ls");
  assertEquals(result.commands!.deny[0].match, "cp");
});

test("mergeEvaluationPolicies - mode redirects override default", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    redirects: { action: "allow" },
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    redirects: { action: "deny", safeTargets: ["/dev/null"] },
  };
  const result = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  assertEquals(result.redirects?.action, "deny");
  assertEquals(result.redirects?.safeTargets, ["/dev/null"]);
});

test("mergeEvaluationPolicies - mode without redirects inherits default", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    redirects: { action: "allow" },
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
  };
  const result = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  assertEquals(result.redirects?.action, "allow");
});

test("mergeEvaluationPolicies - mode heredocs override default", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    heredocs: { action: "allow" },
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    heredocs: { action: "deny" },
  };
  const result = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  assertEquals(result.heredocs?.action, "deny");
});

test("mergeEvaluationPolicies - mode wrappers replace default when present", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    wrappers: [{ name: "bash", kind: "shell-c" }],
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    wrappers: [
      { name: "bash", kind: "shell-c" },
      { name: "sudo", kind: "utility-operand" },
    ],
  };
  const result = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  assertEquals(result.wrappers?.length, 2);
});

test("mergeEvaluationPolicies - mode without wrappers inherits default", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
    wrappers: [{ name: "bash", kind: "shell-c" }],
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [] },
  };
  const result = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  assertEquals(result.wrappers?.length, 1);
  assertEquals(result.wrappers?.[0].name, "bash");
});

test("mergeEvaluationPolicies - mode deny overrides default allow for same command", () => {
  const defaultPolicy: ModePolicy = {
    commands: { allow: [{ match: "cp", mode: "prefix" }], ask: [], deny: [] },
  };
  const modePolicy: ModePolicy = {
    commands: { allow: [], ask: [], deny: [{ match: "cp", mode: "prefix" }] },
  };
  const merged = mergeEvaluationPolicies(defaultPolicy, modePolicy);
  const result = evaluate("cp file1 file2", merged);
  assertEquals(result.action, "deny");
});

// ==================== END MERGE EVALUATION POLICIES TESTS ====================

// ==================== MODE LABEL TESTS ====================
console.log("\n=== Mode Label Tests ===");

import { modeLabel } from "../lib/execution-mode.ts";

test("modeLabel(plan) returns plan mode label", () => {
  assertEquals(modeLabel("plan"), "\uF4A0 plan mode");
});

test("modeLabel(yolo) returns yolo mode label", () => {
  assertEquals(modeLabel("yolo"), "\ueb44 yolo mode");
});

test("modeLabel(edit) returns undefined", () => {
  assertEquals(modeLabel("edit"), undefined);
});

test("modeLabel(unknown) returns undefined", () => {
  assertEquals(modeLabel("unknown"), undefined);
});

// ==================== END MODE LABEL TESTS ====================

// Tests run inline above; print summary
console.log("\n=== Summary ===");
console.log(`${stats.passed} passed, ${stats.failed} failed`);
if (stats.failures.length > 0) {
  console.log("\nFailures:");
  stats.failures.forEach((f) => console.log(`  - ${f}`));
  process.exit(1);
}
