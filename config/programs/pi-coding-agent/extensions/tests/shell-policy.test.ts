/**
 * Comprehensive Test Suite for Shell Policy Engine
 * Run with: nix run nixpkgs#tsx -- shell-policy.test.ts
 */

import {
  tokenize,
  extractCommands,
  evaluateCommand,
  mergePolicies,
  buildWrapperRuleMap,
  type PolicyCommands,
  type EvalResult,
  type WrapperRuleConfig,
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
    if (e instanceof Error && e.message === (msg || "Expected function to throw")) {
      throw e;
    }
    return e as Error;
  }
}

// Provider pattern types
interface TestCase {
  name: string;
  setup?: () => Record<string, unknown>;
  input: Record<string, unknown>;
  expected: Record<string, unknown>;
}

type TestRunner = (testCase: TestCase) => void;

interface TestSuite {
  name: string;
  tests: TestCase[];
  runner: TestRunner;
}

// Test suite registry - collects all test suites to be run later
const testSuites: TestSuite[] = [];

function runTestSuite(suiteName: string, tests: TestCase[], runner: TestRunner): void {
  // Register the test suite for later execution
  testSuites.push({ name: suiteName, tests, runner });
}

function executeTestSuites(): void {
  for (const suite of testSuites) {
    console.log(`\n=== ${suite.name} ===`);
    for (const tc of suite.tests) {
      test(tc.name, () => suite.runner(tc));
    }
  }
}

// ==================== TOKENIZER TESTS ====================

// Basic words
const tokenizerBasicTests: TestCase[] = [
  {
    name: "basic word",
    input: { command: "ls" },
    expected: { length: 1, firstToken: { type: "word", value: "ls" } }
  },
  {
    name: "multiple words",
    input: { command: "ls -la" },
    expected: { length: 2, tokens: [{ type: "word", value: "ls" }, { type: "word", value: "-la" }] }
  },
  {
    name: "echo with arguments",
    input: { command: "echo hello world" },
    expected: { length: 3, tokens: [{ type: "word", value: "echo" }, { type: "word", value: "hello" }, { type: "word", value: "world" }] }
  }
];

runTestSuite("Tokenizer Basic Tests", tokenizerBasicTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.tokens) {
    for (let i = 0; i < tc.expected.tokens.length; i++) {
      assertEquals(tokens[i], tc.expected.tokens[i]);
    }
  }
  if (tc.expected.firstToken) {
    assertEquals(tokens[0], tc.expected.firstToken);
  }
});

// Single quotes
const tokenizerSingleQuoteTests: TestCase[] = [
  {
    name: "single quoted string",
    input: { command: "echo 'hello world'" },
    expected: { length: 2, tokens: [{ type: "word", value: "echo" }, { type: "word", value: "hello world" }] }
  },
  {
    name: "escaped single quote (single quotes don't escape)",
    input: { command: "'it\\'s'" },
    expected: { shouldThrow: true }
  },
  {
    name: "multiple single quoted words",
    input: { command: "'foo' 'bar'" },
    expected: { length: 2, tokens: [{ type: "word", value: "foo" }, { type: "word", value: "bar" }] }
  }
];

runTestSuite("Tokenizer Single Quote Tests", tokenizerSingleQuoteTests, (tc) => {
  if (tc.expected.shouldThrow) {
    assertThrows(() => tokenize(tc.input.command as string));
    return;
  }
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.tokens) {
    for (let i = 0; i < tc.expected.tokens.length; i++) {
      assertEquals(tokens[i], tc.expected.tokens[i]);
    }
  }
});

// Double quotes
const tokenizerDoubleQuoteTests: TestCase[] = [
  {
    name: "double quoted string",
    input: { command: "echo \"hello world\"" },
    expected: { length: 2, secondToken: { type: "word", value: "hello world" } }
  },
  {
    name: "escaped double quote",
    input: { command: "\"say \\\"hi\\\"\"" },
    expected: { length: 1, firstToken: { type: "word", value: 'say "hi"' } }
  },
  {
    name: "double quote with variable reference",
    input: { command: "\"value is $VAR\"" },
    expected: { length: 1, firstToken: { type: "word", value: "value is $VAR" } }
  }
];

runTestSuite("Tokenizer Double Quote Tests", tokenizerDoubleQuoteTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.firstToken) {
    assertEquals(tokens[0], tc.expected.firstToken);
  }
  if (tc.expected.secondToken) {
    assertEquals(tokens[1], tc.expected.secondToken);
  }
});

// Escape sequences
const tokenizerEscapeTests: TestCase[] = [
  {
    name: "escaped space",
    input: { command: "hello\\ world" },
    expected: { length: 1, firstToken: { type: "word", value: "hello world" } }
  },
  {
    name: "escaped newline (line continuation)",
    input: { command: "echo \\\n  hello" },
    expected: { length: 2, tokens: [{ type: "word", value: "echo" }, { type: "word", value: "hello" }] }
  }
];

runTestSuite("Tokenizer Escape Tests", tokenizerEscapeTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.tokens) {
    for (let i = 0; i < tc.expected.tokens.length; i++) {
      assertEquals(tokens[i], tc.expected.tokens[i]);
    }
  }
  if (tc.expected.firstToken) {
    assertEquals(tokens[0], tc.expected.firstToken);
  }
});

// Operators
const tokenizerOperatorTests: TestCase[] = [
  {
    name: "pipe operator",
    input: { command: "cat file | grep hi" },
    expected: { length: 5, operatorAt: { index: 2, value: "|" } }
  },
  {
    name: "logical AND operator",
    input: { command: "cmd1 && cmd2" },
    expected: { length: 3, operatorAt: { index: 1, value: "&&" } }
  },
  {
    name: "logical OR operator",
    input: { command: "cmd1 || cmd2" },
    expected: { length: 3, operatorAt: { index: 1, value: "||" } }
  },
  {
    name: "semicolon operator",
    input: { command: "cmd1 ; cmd2" },
    expected: { length: 3, operatorAt: { index: 1, value: ";" } }
  },
  {
    name: "background operator",
    input: { command: "cmd &" },
    expected: { length: 2, operatorAt: { index: 1, value: "&" } }
  },
  {
    name: "mixed operators",
    input: { command: "a && b || c ; d &" },
    expected: { operatorCount: 4 }
  }
];

runTestSuite("Tokenizer Operator Tests", tokenizerOperatorTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  if (tc.expected.length) {
    assertEquals(tokens.length, tc.expected.length);
  }
  if (tc.expected.operatorAt) {
    assertEquals(tokens[tc.expected.operatorAt.index], { type: "operator", value: tc.expected.operatorAt.value });
  }
  if (tc.expected.operatorCount) {
    assertEquals(tokens.filter((t) => t.type === "operator").length, tc.expected.operatorCount);
  }
});

// Groups - subshells
const tokenizerSubshellTests: TestCase[] = [
  {
    name: "subshell group",
    input: { command: "(echo hi)" },
    expected: { length: 1, firstTokenType: "group" }
  },
  {
    name: "subshell with multiple commands",
    input: { command: "(echo a && echo b)" },
    expected: { length: 1, firstTokenType: "group" }
  }
];

runTestSuite("Tokenizer Subshell Tests", tokenizerSubshellTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.firstTokenType) {
    assertEquals(tokens[0].type, tc.expected.firstTokenType);
  }
});

// Groups - command substitution
const tokenizerSubstitutionTests: TestCase[] = [
  {
    name: "dollar substitution",
    input: { command: "$(echo hi)" },
    expected: { length: 1, firstTokenType: "group" }
  },
  {
    name: "backtick substitution",
    input: { command: "`echo hi`" },
    expected: { length: 1, firstTokenType: "group" }
  },
  {
    name: "nested substitution",
    input: { command: "$(echo $(echo hi))" },
    expected: { length: 1, firstTokenType: "group" }
  }
];

runTestSuite("Tokenizer Substitution Tests", tokenizerSubstitutionTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.firstTokenType) {
    assertEquals(tokens[0].type, tc.expected.firstTokenType);
  }
});

// Redirections
const tokenizerRedirectTests: TestCase[] = [
  {
    name: "output redirect",
    input: { command: "echo hi > file.txt" },
    expected: { length: 3, redirectAt: { index: 2, op: ">", target: "file.txt" } }
  },
  {
    name: "append redirect",
    input: { command: "echo hi >> file.txt" },
    expected: { redirectAt: { index: 2, op: ">>", target: "file.txt" } }
  },
  {
    name: "input redirect",
    input: { command: "cat < file.txt" },
    expected: { length: 2, redirectAt: { index: 1, op: "<", target: "file.txt" } }
  },
  {
    name: "heredoc redirect",
    input: { command: "cat <<EOF" },
    expected: { redirectAt: { index: 1, op: "<<", target: "EOF" } }
  },
  {
    name: "heredoc with strip redirect",
    input: { command: "cat <<-EOF" },
    expected: { redirectAt: { index: 1, op: "<<-", target: "EOF" } }
  },
  {
    name: "here-string redirect",
    input: { command: "cat <<<'hello'" },
    expected: { redirectAt: { index: 1, op: "<<<", target: "hello" } }
  },
  {
    name: "fd output redirect",
    input: { command: "cmd 2> file.txt" },
    expected: { redirectAt: { index: 1, op: "2>", target: "file.txt" } }
  },
  {
    name: "fd append redirect",
    input: { command: "cmd 2>> file.txt" },
    expected: { redirectAt: { index: 1, op: "2>>", target: "file.txt" } }
  },
  {
    name: "fd duplication redirect",
    input: { command: "cmd 2>&1" },
    expected: { redirectAt: { index: 1, op: "2>&", target: "1" } }
  },
  {
    name: "input duplication redirect (parsed as separate tokens)",
    input: { command: "cmd <&0" },
    expected: { 
      length: 4,
      tokens: [
        { type: "word", value: "cmd" },
        { type: "redirect", op: "<", target: "" },
        { type: "operator", value: "&" },
        { type: "word", value: "0" }
      ]
    }
  },
  {
    name: "multiple redirects",
    input: { command: "cmd > out.txt 2> err.txt" },
    expected: { redirectCount: 2 }
  }
];

runTestSuite("Tokenizer Redirect Tests", tokenizerRedirectTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  if (tc.expected.length) {
    assertEquals(tokens.length, tc.expected.length);
  }
  if (tc.expected.redirectAt) {
    assertEquals(tokens[tc.expected.redirectAt.index], { type: "redirect", op: tc.expected.redirectAt.op, target: tc.expected.redirectAt.target });
  }
  if (tc.expected.redirectCount) {
    assertEquals(tokens.filter((t) => t.type === "redirect").length, tc.expected.redirectCount);
  }
  if (tc.expected.tokens) {
    for (let i = 0; i < tc.expected.tokens.length; i++) {
      assertEquals(tokens[i], tc.expected.tokens[i]);
    }
  }
});

// Comments
const tokenizerCommentTests: TestCase[] = [
  {
    name: "comment ignored",
    input: { command: "echo hi # this is ignored" },
    expected: { length: 2, tokens: [{ type: "word", value: "echo" }, { type: "word", value: "hi" }] }
  },
  {
    name: "comment after redirect",
    input: { command: "echo hi > file # comment" },
    expected: { length: 3 }
  }
];

runTestSuite("Tokenizer Comment Tests", tokenizerCommentTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.tokens) {
    for (let i = 0; i < tc.expected.tokens.length; i++) {
      assertEquals(tokens[i], tc.expected.tokens[i]);
    }
  }
});

// Variable substitution
const tokenizerVariableTests: TestCase[] = [
  {
    name: "variable not parsed as standalone word",
    input: { command: "echo $VAR" },
    expected: { minLength: 1, firstToken: { type: "word", value: "echo" } }
  }
];

runTestSuite("Tokenizer Variable Tests", tokenizerVariableTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertTrue(tokens.length >= (tc.expected.minLength as number));
  if (tc.expected.firstToken) {
    assertEquals(tokens[0], tc.expected.firstToken);
  }
});

// Complex combinations
const tokenizerComplexTests: TestCase[] = [
  {
    name: "pipeline with redirects",
    input: { command: "echo \"hello\" > file.txt && cat < file.txt | grep hi" },
    expected: { length: 9, operatorCount: 2, redirectCount: 2 }
  },
  {
    name: "complex with groups",
    input: { command: "echo $(date) && (ls -la) | wc -l" },
    expected: { groupCount: 2 }
  }
];

runTestSuite("Tokenizer Complex Tests", tokenizerComplexTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  if (tc.expected.length) {
    assertEquals(tokens.length, tc.expected.length);
  }
  if (tc.expected.operatorCount) {
    assertEquals(tokens.filter((t) => t.type === "operator").length, tc.expected.operatorCount);
  }
  if (tc.expected.redirectCount) {
    assertEquals(tokens.filter((t) => t.type === "redirect").length, tc.expected.redirectCount);
  }
  if (tc.expected.groupCount) {
    assertEquals(tokens.filter((t) => t.type === "group").length, tc.expected.groupCount);
  }
});

// Error cases
const tokenizerErrorTests: TestCase[] = [
  {
    name: "throws on unmatched single quote",
    input: { command: "echo 'unclosed" },
    expected: { shouldThrow: true }
  },
  {
    name: "throws on unmatched double quote",
    input: { command: "echo \"unclosed" },
    expected: { shouldThrow: true }
  },
  {
    name: "throws on unmatched subshell",
    input: { command: "(echo hi" },
    expected: { shouldThrow: true }
  },
  {
    name: "throws on unmatched substitution",
    input: { command: "$(echo hi" },
    expected: { shouldThrow: true }
  },
  {
    name: "throws on unmatched backtick",
    input: { command: "`echo hi" },
    expected: { shouldThrow: true }
  }
];

runTestSuite("Tokenizer Error Tests", tokenizerErrorTests, (tc) => {
  assertThrows(() => tokenize(tc.input.command as string));
});

// ==================== END TOKENIZER TESTS ====================

// ==================== COMMAND EXTRACTION TESTS ====================

// Basic commands
const commandExtractionBasicTests: TestCase[] = [
  {
    name: "basic command",
    input: { command: "ls -la" },
    expected: { 
      length: 1, 
      firstCommand: { name: "ls", fullText: "ls -la", source: "direct" }
    }
  },
  {
    name: "command with redirects",
    input: { command: "echo hi > file.txt" },
    expected: { 
      length: 1, 
      firstCommand: { name: "echo", redirectCount: 1, firstRedirect: { op: ">", target: "file.txt" } }
    }
  },
  {
    name: "command with multiple redirects",
    input: { command: "cmd > out.txt 2> err.txt" },
    expected: { firstCommand: { redirectCount: 2 } }
  }
];

runTestSuite("Command Extraction Basic Tests", commandExtractionBasicTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  if (tc.expected.length) {
    assertEquals(cmds.length, tc.expected.length);
  }
  if (tc.expected.firstCommand) {
    const fc = tc.expected.firstCommand;
    if (fc.name) assertEquals(cmds[0].name, fc.name);
    if (fc.fullText) assertEquals(cmds[0].fullText, fc.fullText);
    if (fc.source) assertEquals(cmds[0].source, fc.source);
    if (fc.redirectCount) assertEquals(cmds[0].redirects.length, fc.redirectCount);
    if (fc.firstRedirect) assertEquals(cmds[0].redirects[0], fc.firstRedirect);
  }
});

// Multiple segments with control operators
const commandExtractionControlTests: TestCase[] = [
  {
    name: "multiple with &&",
    input: { command: "echo a && echo b" },
    expected: { 
      length: 2, 
      commands: [
        { name: "echo", fullText: "echo a" },
        { name: "echo", fullText: "echo b" }
      ]
    }
  },
  {
    name: "multiple with ||",
    input: { command: "cmd1 || cmd2" },
    expected: { length: 2, commands: [{ name: "cmd1" }, { name: "cmd2" }] }
  },
  {
    name: "multiple with ;",
    input: { command: "cmd1 ; cmd2 ; cmd3" },
    expected: { length: 3 }
  }
];

runTestSuite("Command Extraction Control Tests", commandExtractionControlTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, tc.expected.length);
  if (tc.expected.commands) {
    for (let i = 0; i < tc.expected.commands.length; i++) {
      const exp = tc.expected.commands[i];
      if (exp.name) assertEquals(cmds[i].name, exp.name);
      if (exp.fullText) assertEquals(cmds[i].fullText, exp.fullText);
    }
  }
});

// Pipelines
const commandExtractionPipelineTests: TestCase[] = [
  {
    name: "simple pipeline",
    input: { command: "cat file | grep hi" },
    expected: { length: 2, names: ["cat", "grep"] }
  },
  {
    name: "multi-stage pipeline",
    input: { command: "cat file | grep hi | sort | uniq" },
    expected: { length: 4, names: ["cat", "grep", "sort", "uniq"] }
  }
];

runTestSuite("Command Extraction Pipeline Tests", commandExtractionPipelineTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, tc.expected.length);
  if (tc.expected.names) {
    assertEquals(cmds.map((c) => c.name), tc.expected.names);
  }
});

// Subshells
const commandExtractionSubshellTests: TestCase[] = [
  {
    name: "subshell with single command",
    input: { command: "(echo hi)" },
    expected: { subshellCount: 1, subshellNames: ["echo"] }
  },
  {
    name: "subshell with multiple commands",
    input: { command: "(echo a && echo b)" },
    expected: { subshellCount: 2 }
  },
  {
    name: "subshell with pipeline",
    input: { command: "(cat file | grep hi)" },
    expected: { subshellCount: 2 }
  }
];

runTestSuite("Command Extraction Subshell Tests", commandExtractionSubshellTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  const subshellCmds = cmds.filter((c) => c.source === "subshell");
  assertEquals(subshellCmds.length, tc.expected.subshellCount);
  if (tc.expected.subshellNames) {
    assertEquals(subshellCmds.map((c) => c.name), tc.expected.subshellNames);
  }
});

// Command substitution
const commandExtractionSubstitutionTests: TestCase[] = [
  {
    name: "command substitution $(...)",
    input: { command: "echo $(date)" },
    expected: { length: 2, hasCommand: { name: "date", source: "substitution" } }
  },
  {
    name: "backtick substitution",
    input: { command: "echo `date`" },
    expected: { length: 2, hasCommand: { name: "date", source: "substitution" } }
  },
  {
    name: "nested substitution",
    input: { command: "echo $(echo $(date))" },
    expected: { minLength: 2, hasCommand: { name: "date" } }
  }
];

runTestSuite("Command Extraction Substitution Tests", commandExtractionSubstitutionTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  if (tc.expected.length) {
    assertEquals(cmds.length, tc.expected.length);
  }
  if (tc.expected.minLength) {
    assertTrue(cmds.length >= tc.expected.minLength);
  }
  if (tc.expected.hasCommand) {
    const cmd = cmds.find((c) => c.name === tc.expected.hasCommand.name);
    assertTrue(cmd !== undefined);
    if (tc.expected.hasCommand.source) {
      assertEquals(cmd?.source, tc.expected.hasCommand.source);
    }
  }
});

// Wrapper commands
const commandExtractionWrapperTests: TestCase[] = [
  {
    name: "bash -c wrapper",
    input: { command: "bash -c 'echo hi'" },
    expected: { length: 2, hasCommand: { name: "echo", source: "wrapper-arg" } }
  },
  {
    name: "sh -c wrapper",
    input: { command: "sh -c 'ls -la'" },
    expected: { hasCommand: { name: "ls", source: "wrapper-arg" } }
  },
  {
    name: "sudo passthrough",
    input: { command: "sudo rm -rf /" },
    expected: { 
      length: 2, 
      commands: [
        { name: "sudo", source: "direct" },
        { name: "rm", source: "wrapper-arg" }
      ]
    }
  },
  {
    name: "doas passthrough",
    input: { command: "doas ls -la" },
    expected: { length: 2, secondCommand: { name: "ls" } }
  },
  {
    name: "xargs passthrough",
    input: { command: "xargs rm" },
    expected: { length: 2, secondCommand: { name: "rm" } }
  },
  {
    name: "time passthrough without --",
    input: { command: "time sleep 1" },
    expected: { length: 2, secondCommand: { name: "sleep" } }
  },
  {
    name: "time -- passthrough respects -- end-of-options",
    input: { command: "time -- echo hi" },
    expected: { length: 2, secondCommand: { name: "echo" } }
  },
  {
    name: "env wrapper handles -- before command",
    input: { command: "env -- VAR=val cmd arg" },
    expected: { 
      length: 2, 
      secondCommand: { name: "cmd", fullText: "cmd arg" }
    }
  },
  {
    name: "env wrapper skips assignments",
    input: { command: "env VAR=val echo hi" },
    expected: { length: 2, secondCommand: { name: "echo" } }
  },
  {
    name: "env with multiple assignments",
    input: { command: "env A=1 B=2 C=3 cmd arg" },
    expected: { 
      length: 2, 
      secondCommand: { name: "cmd", fullText: "cmd arg" }
    }
  },
  {
    name: "nohup wrapper extracts utility operand",
    input: { command: "nohup cat file.txt" },
    expected: { length: 2, secondCommand: { name: "cat" } }
  }
];

runTestSuite("Command Extraction Wrapper Tests", commandExtractionWrapperTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  if (tc.expected.length) {
    assertEquals(cmds.length, tc.expected.length);
  }
  if (tc.expected.commands) {
    for (let i = 0; i < tc.expected.commands.length; i++) {
      const exp = tc.expected.commands[i];
      if (exp.name) assertEquals(cmds[i].name, exp.name);
      if (exp.source) assertEquals(cmds[i].source, exp.source);
    }
  }
  if (tc.expected.secondCommand) {
    const sc = tc.expected.secondCommand;
    if (sc.name) assertEquals(cmds[1].name, sc.name);
    if (sc.source) assertEquals(cmds[1].source, sc.source);
    if (sc.fullText) assertEquals(cmds[1].fullText, sc.fullText);
  }
  if (tc.expected.hasCommand) {
    const cmd = cmds.find((c) => c.name === tc.expected.hasCommand.name);
    assertTrue(cmd !== undefined);
    if (tc.expected.hasCommand.source) {
      assertEquals(cmd?.source, tc.expected.hasCommand.source);
    }
  }
});

// Nested wrappers
const commandExtractionNestedTests: TestCase[] = [
  {
    name: "nested wrappers bash -c with sudo",
    input: { command: "sudo bash -c 'rm -rf /'" },
    expected: { length: 3, hasCommand: { name: "rm" } }
  },
  {
    name: "deeply nested",
    input: { command: "echo $(bash -c 'ls $(pwd)')" },
    expected: { minLength: 3, hasCommands: ["bash", "ls", "pwd"] }
  }
];

runTestSuite("Command Extraction Nested Tests", commandExtractionNestedTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  if (tc.expected.length) {
    assertEquals(cmds.length, tc.expected.length);
  }
  if (tc.expected.minLength) {
    assertTrue(cmds.length >= tc.expected.minLength);
  }
  if (tc.expected.hasCommand) {
    const cmd = cmds.find((c) => c.name === tc.expected.hasCommand.name);
    assertTrue(cmd !== undefined);
  }
  if (tc.expected.hasCommands) {
    for (const name of tc.expected.hasCommands) {
      assertTrue(cmds.some((c) => c.name === name));
    }
  }
});

// Complex cases
const commandExtractionComplexTests: TestCase[] = [
  {
    name: "mixed pipeline and subshell",
    input: { command: "(cat file) | grep hi" },
    expected: { 
      length: 2, 
      commands: [
        { name: "cat", source: "subshell" },
        { name: "grep", source: "direct" }
      ]
    }
  },
  {
    name: "command with empty segment after &&",
    input: { command: "echo || ls" },
    expected: { length: 2 }
  }
];

runTestSuite("Command Extraction Complex Tests", commandExtractionComplexTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, tc.expected.length);
  if (tc.expected.commands) {
    for (let i = 0; i < tc.expected.commands.length; i++) {
      const exp = tc.expected.commands[i];
      if (exp.name) assertEquals(cmds[i].name, exp.name);
      if (exp.source) assertEquals(cmds[i].source, exp.source);
    }
  }
});

// ==================== END COMMAND EXTRACTION TESTS ====================

// ==================== POLICY MATCHING TESTS ====================

// Helper for creating simple policies
const samplePolicy: PolicyCommands = {
  allow: [{ match: "ls", mode: "prefix" }, { match: "echo", mode: "prefix" }],
  ask: [{ match: "rm", mode: "prefix" }],
  deny: [{ match: "sudo", mode: "prefix" }],
};

// Exact match mode
const exactMatchTests: TestCase[] = [
  {
    name: "exact match - matches",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "ls", mode: "exact" }] } as PolicyCommands
    }),
    input: { command: "ls" },
    expected: { action: "deny" }
  },
  {
    name: "exact match - case insensitive",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "LS", mode: "exact" }] } as PolicyCommands
    }),
    input: { command: "ls" },
    expected: { action: "deny" }
  },
  {
    name: "exact match - whitespace trimmed",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "ls", mode: "exact" }] } as PolicyCommands
    }),
    input: { command: "  ls  " },
    expected: { action: "deny" }
  },
  {
    name: "exact match - args don't match",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "ls", mode: "exact" }] } as PolicyCommands
    }),
    input: { command: "ls -la" },
    expected: { action: "default" }
  }
];

runTestSuite("Exact Match Tests", exactMatchTests, (tc) => {
  const { policy } = tc.setup ? tc.setup() : { policy: samplePolicy };
  const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
  assertEquals(result.action, tc.expected.action);
});

// Prefix match mode
const prefixMatchTests: TestCase[] = [
  {
    name: "prefix match - matches",
    input: { command: "ls -la" },
    expected: { action: "allow" }
  },
  {
    name: "prefix match - exact also matches",
    input: { command: "ls" },
    expected: { action: "allow" }
  },
  {
    name: "prefix match - case insensitive",
    input: { command: "LS -LA" },
    expected: { action: "allow" }
  },
  {
    name: "prefix match - no match when pattern longer",
    input: { command: "l" },
    expected: { action: "default" }
  }
];

runTestSuite("Prefix Match Tests", prefixMatchTests, (tc) => {
  const result = evaluateCommand(tc.input.command as string, samplePolicy);
  assertEquals(result.action, tc.expected.action);
});

// Substring match mode
const substringMatchTests: TestCase[] = [
  {
    name: "substring match - exact",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "rm -rf /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "rm -rf /" },
    expected: { action: "deny" }
  },
  {
    name: "substring match - in pipeline",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "rm -rf /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "echo hi && rm -rf /" },
    expected: { action: "deny" }
  },
  {
    name: "substring match - partial word",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "rm -rf", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "grm -rf file" },
    expected: { action: "deny" }
  },
  {
    name: "substring match - case insensitive",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "RM -RF /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "echo hi && rm -rf /" },
    expected: { action: "deny" }
  },
  {
    name: "substring match - special characters",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "git push", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "git push origin main" },
    expected: { action: "deny" }
  }
];

runTestSuite("Substring Match Tests", substringMatchTests, (tc) => {
  const { policy } = tc.setup ? tc.setup() : { policy: samplePolicy };
  const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
  assertEquals(result.action, tc.expected.action);
});

// has-redirect match mode
const hasRedirectTests: TestCase[] = [
  {
    name: "has-redirect - output redirect",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "*", mode: "has-redirect" }] } as PolicyCommands
    }),
    input: { command: "echo hi > file.txt" },
    expected: { action: "deny" }
  },
  {
    name: "has-redirect - append redirect",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "*", mode: "has-redirect" }] } as PolicyCommands
    }),
    input: { command: "echo hi >> file.txt" },
    expected: { action: "deny" }
  },
  {
    name: "has-redirect - input redirect not matched",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "*", mode: "has-redirect" }] } as PolicyCommands
    }),
    input: { command: "cat < file.txt" },
    expected: { action: "default" }
  },
  {
    name: "has-redirect - /dev/null excluded",
    setup: () => ({
      policy: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [{ match: "*", mode: "has-redirect" }] } as PolicyCommands
    }),
    input: { command: "ls 2>/dev/null" },
    expected: { action: "allow" }
  },
  {
    name: "has-redirect - fd duplication excluded",
    setup: () => ({
      policy: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [{ match: "*", mode: "has-redirect" }] } as PolicyCommands
    }),
    input: { command: "ls 2>&1" },
    expected: { action: "allow" }
  },
  {
    name: "has-redirect - combined safe redirects",
    setup: () => ({
      policy: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [{ match: "*", mode: "has-redirect" }] } as PolicyCommands
    }),
    input: { command: "ls 2>&1 >/dev/null" },
    expected: { action: "allow" }
  }
];

runTestSuite("Has-Redirect Match Tests", hasRedirectTests, (tc) => {
  const { policy } = tc.setup ? tc.setup() : { policy: samplePolicy };
  const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
  assertEquals(result.action, tc.expected.action);
});

// has-heredoc match mode
const hasHeredocTests: TestCase[] = [
  {
    name: "has-heredoc - matches heredoc",
    setup: () => ({
      policy: { allow: [], ask: [{ match: "*", mode: "has-heredoc" }], deny: [] } as PolicyCommands
    }),
    input: { command: "cat <<EOF" },
    expected: { action: "ask" }
  },
  {
    name: "has-heredoc - matches stripping variant",
    setup: () => ({
      policy: { allow: [], ask: [{ match: "*", mode: "has-heredoc" }], deny: [] } as PolicyCommands
    }),
    input: { command: "cat <<-EOF" },
    expected: { action: "ask" }
  },
  {
    name: "has-heredoc - no match without heredoc",
    setup: () => ({
      policy: { allow: [], ask: [{ match: "*", mode: "has-heredoc" }], deny: [] } as PolicyCommands
    }),
    input: { command: "cat file.txt" },
    expected: { action: "default" }
  },
  {
    name: "has-heredoc - here-string not matched",
    setup: () => ({
      policy: { allow: [], ask: [{ match: "*", mode: "has-heredoc" }], deny: [] } as PolicyCommands
    }),
    input: { command: "cat <<<'hello'" },
    expected: { action: "default" }
  }
];

runTestSuite("Has-Heredoc Match Tests", hasHeredocTests, (tc) => {
  const { policy } = tc.setup ? tc.setup() : { policy: samplePolicy };
  const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
  assertEquals(result.action, tc.expected.action);
});

// Priority tests
const priorityTests: TestCase[] = [
  {
    name: "deny takes priority over allow",
    setup: () => ({
      policy: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [{ match: "ls", mode: "prefix" }] } as PolicyCommands
    }),
    input: { command: "ls -la" },
    expected: { action: "deny" }
  },
  {
    name: "deny takes priority over ask",
    setup: () => ({
      policy: { allow: [], ask: [{ match: "sudo", mode: "prefix" }], deny: [{ match: "sudo", mode: "prefix" }] } as PolicyCommands
    }),
    input: { command: "sudo ls" },
    expected: { action: "deny" }
  },
  {
    name: "ask escalates over allow",
    input: { command: "echo hi && rm file" },
    expected: { action: "ask" }
  },
  {
    name: "multiple commands - one deny triggers deny",
    setup: () => ({
      policy: { allow: [{ match: "echo", mode: "prefix" }], ask: [], deny: [{ match: "rm -rf /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "echo hi && rm -rf /" },
    expected: { action: "deny" }
  }
];

runTestSuite("Priority Tests", priorityTests, (tc) => {
  const { policy } = tc.setup ? tc.setup() : { policy: samplePolicy };
  const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
  assertEquals(result.action, tc.expected.action);
});

// Wrapper command extraction
const wrapperTests: TestCase[] = [
  {
    name: "wrapper command extraction - sudo denied",
    input: { command: "sudo ls -la" },
    expected: { action: "deny" }
  },
  {
    name: "wrapper command extraction - sudo prefix match",
    input: { command: "sudo ls" },
    expected: { action: "deny" }
  },
  {
    name: "wrapper command extraction - bash -c wrapper",
    setup: () => ({
      policy: { allow: [{ match: "echo", mode: "prefix" }], ask: [], deny: [{ match: "rm", mode: "prefix" }] } as PolicyCommands
    }),
    input: { command: "bash -c 'rm -rf /'" },
    expected: { action: "deny" }
  },
  {
    name: "wrapper command extraction - nested wrappers",
    input: { command: "sudo bash -c 'ls -la'" },
    expected: { action: "deny" }
  }
];

runTestSuite("Wrapper Command Tests", wrapperTests, (tc) => {
  const { policy } = tc.setup ? tc.setup() : { policy: samplePolicy };
  const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
  assertEquals(result.action, tc.expected.action);
});

// Error handling
const errorHandlingTests: TestCase[] = [
  {
    name: "parse error returns ask",
    input: { command: "echo 'unclosed" },
    expected: { action: "ask", reasonIncludes: "Unparseable" }
  },
  {
    name: "empty command returns ask",
    input: { command: "" },
    expected: { action: "ask" }
  },
  {
    name: "whitespace-only command returns ask",
    input: { command: "   \n\t  " },
    expected: { action: "ask" }
  },
  {
    name: "unknown command returns default",
    input: { command: "unknown_cmd" },
    expected: { action: "default" }
  }
];

runTestSuite("Error Handling Tests", errorHandlingTests, (tc) => {
  const result = evaluateCommand(tc.input.command as string, samplePolicy);
  assertEquals(result.action, tc.expected.action);
  if (tc.expected.reasonIncludes) {
    assertTrue(result.reason?.includes(tc.expected.reasonIncludes as string));
  }
});

// ==================== END POLICY MATCHING TESTS ====================

// ==================== MERGE POLICIES TESTS ====================

const mergePoliciesTests: TestCase[] = [
  {
    name: "combines all sections",
    input: {
      p1: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands,
      p2: { allow: [], ask: [{ match: "rm", mode: "prefix" }], deny: [{ match: "sudo", mode: "prefix" }] } as PolicyCommands
    },
    expected: { allow: 1, ask: 1, deny: 1, firstAllow: "ls", firstAsk: "rm", firstDeny: "sudo" }
  },
  {
    name: "combines multiple policies",
    input: {
      policies: [
        { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands,
        { allow: [{ match: "cat", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands,
        { allow: [{ match: "grep", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands
      ]
    },
    expected: { allow: 3, matches: ["ls", "cat", "grep"] }
  },
  {
    name: "empty policies",
    input: {
      p1: { allow: [], ask: [], deny: [] } as PolicyCommands,
      p2: { allow: [{ match: "ls", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands
    },
    expected: { allow: 1, ask: 0, deny: 0 }
  },
  {
    name: "both empty",
    input: {
      p1: { allow: [], ask: [], deny: [] } as PolicyCommands,
      p2: { allow: [], ask: [], deny: [] } as PolicyCommands
    },
    expected: { allow: 0, ask: 0, deny: 0 }
  },
  {
    name: "order preservation",
    input: {
      policies: [
        { allow: [{ match: "a", mode: "exact" }], ask: [], deny: [] } as PolicyCommands,
        { allow: [{ match: "b", mode: "exact" }], ask: [], deny: [] } as PolicyCommands,
        { allow: [{ match: "c", mode: "exact" }], ask: [], deny: [] } as PolicyCommands
      ]
    },
    expected: { matches: ["a", "b", "c"] }
  }
];

runTestSuite("Merge Policies Tests", mergePoliciesTests, (tc) => {
  let merged: PolicyCommands;
  if (tc.input.policies) {
    merged = mergePolicies(...(tc.input.policies as PolicyCommands[]));
  } else {
    merged = mergePolicies(tc.input.p1 as PolicyCommands, tc.input.p2 as PolicyCommands);
  }
  if (tc.expected.allow !== undefined) assertEquals(merged.allow.length, tc.expected.allow);
  if (tc.expected.ask !== undefined) assertEquals(merged.ask.length, tc.expected.ask);
  if (tc.expected.deny !== undefined) assertEquals(merged.deny.length, tc.expected.deny);
  if (tc.expected.firstAllow) assertEquals(merged.allow[0].match, tc.expected.firstAllow);
  if (tc.expected.firstAsk) assertEquals(merged.ask[0].match, tc.expected.firstAsk);
  if (tc.expected.firstDeny) assertEquals(merged.deny[0].match, tc.expected.firstDeny);
  if (tc.expected.matches) assertEquals(merged.allow.map((e) => e.match), tc.expected.matches);
});

// ==================== END MERGE POLICIES TESTS ====================

// ==================== EDGE CASE & REGRESSION TESTS ====================

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
    { match: "*", mode: "has-heredoc" },
  ],
  deny: [
    { match: "sudo", mode: "prefix" },
    { match: "doas", mode: "prefix" },
    { match: "git push", mode: "substring" },
    { match: "rm -rf /", mode: "substring" },
    { match: "gh api --method POST", mode: "substring" },
    { match: "gh api --method PUT", mode: "substring" },
    { match: "gh api --method DELETE", mode: "substring" },
    { match: "*", mode: "has-redirect" },
  ],
};

// Production policy tests
const productionPolicyTests: TestCase[] = [
  { name: "git push denied", input: { command: "git push origin main" }, expected: { action: "deny" } },
  { name: "git status allowed", input: { command: "git status" }, expected: { action: "default" } },
  { name: "rm -rf / denied", input: { command: "rm -rf /" }, expected: { action: "deny" } },
  { name: "rm -rf / in text denied", input: { command: "echo hi && rm -rf /" }, expected: { action: "deny" } },
  { name: "chmod asks", input: { command: "chmod +x script.sh" }, expected: { action: "ask" } },
  { name: "rm asks", input: { command: "rm file.txt" }, expected: { action: "ask" } },
  { name: "docker exec asks", input: { command: "docker exec -it container bash" }, expected: { action: "ask" } },
  { name: "nix run asks", input: { command: "nix run nixpkgs#something" }, expected: { action: "ask" } },
  { name: "jj describe asks", input: { command: "jj describe -m 'update'" }, expected: { action: "ask" } },
  { name: "gh api POST pattern not matching", input: { command: "gh api repos/foo --method POST" }, expected: { action: "default" } },
  { name: "gh api GET allowed", input: { command: "gh api repos/foo" }, expected: { action: "default" } },
  { name: "sudo denied", input: { command: "sudo ls -la" }, expected: { action: "deny" } },
  { name: "safe redirect to /dev/null allowed", input: { command: "ls 2>/dev/null" }, expected: { action: "allow" } },
  { name: "redirect to file denied", input: { command: "echo hi > file.txt" }, expected: { action: "deny" } },
  { name: "heredoc asks", input: { command: "cat <<EOF\nhello\nEOF" }, expected: { action: "ask" } },
  { name: "quoted > not redirect", input: { command: "jq '.x > .y' file.json" }, expected: { action: "default" } },
  { name: "echo with quoted > allowed", input: { command: "echo 'hello > world'" }, expected: { action: "allow" } },
  { name: "grep with quoted > allowed", input: { command: "grep '>' file.txt" }, expected: { action: "allow" } }
];

runTestSuite("Production Policy Tests", productionPolicyTests, (tc) => {
  const result = evaluateCommand(tc.input.command as string, productionPolicy);
  assertEquals(result.action, tc.expected.action);
});

// Regression tests
const regressionTests: TestCase[] = [
  { name: "bash -c git push denied", input: { command: "bash -c 'git push'" }, expected: { action: "deny" } },
  { name: "echo with quoted rm -rf / - substring matches", input: { command: "echo 'rm -rf /'" }, expected: { action: "deny" } },
  { name: "grep with sudo pattern allowed", input: { command: "grep 'sudo' file.txt" }, expected: { action: "allow" } },
  { name: "bash -c rm -rf / denied", input: { command: "bash -c 'rm -rf /'" }, expected: { action: "deny" } },
  { name: "sudo in substitution denied", input: { command: "$(sudo reboot)" }, expected: { action: "deny" } }
];

runTestSuite("Regression Tests", regressionTests, (tc) => {
  const result = evaluateCommand(tc.input.command as string, productionPolicy);
  assertEquals(result.action, tc.expected.action);
});

// Complex edge cases - tokenization/extraction
const edgeCaseComplexTests: TestCase[] = [
  { name: "deeply nested subshells", input: { command: "$(echo $(echo $(echo hi)))" }, expected: { minCommands: 3 } },
  { name: "wrapper in wrapper", input: { command: "sudo bash -c 'sh -c \"echo hi\"'" }, expected: { hasCommands: ["sudo", "bash"] } },
  { name: "complex pipeline with subshells", input: { command: "(cat a; cat b) | grep x | (sort | uniq)" }, expected: { minCommands: 5 } },
  { name: "multiple && || combinations", input: { command: "cmd1 && cmd2 || cmd3 && cmd4" }, expected: { commandCount: 4 } },
  { name: "long command chain", input: { command: "a | b | c | d | e | f | g" }, expected: { commandCount: 7 } }
];

runTestSuite("Edge Case Complex Tests", edgeCaseComplexTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  if (tc.expected.minCommands) assertTrue(cmds.length >= tc.expected.minCommands);
  if (tc.expected.commandCount) assertEquals(cmds.length, tc.expected.commandCount);
  if (tc.expected.hasCommands) {
    for (const name of tc.expected.hasCommands) {
      assertTrue(cmds.some((c) => c.name === name), `Expected command ${name} not found`);
    }
  }
});

// Edge cases - redirects
const edgeCaseRedirectTests: TestCase[] = [
  { name: "redirects in various positions", input: { command: "cmd > out 2> err < input" }, expected: { redirectCount: 3 } },
  { name: "quoted heredoc delimiter", input: { command: "cat <<'EOF'\nhello\nEOF" }, expected: { tokenAt: { index: 1, value: { type: "redirect", op: "<<", target: "EOF" } } } },
  { name: "escaped characters in arguments", input: { command: "echo 'foo*bar'" }, expected: { tokenAt: { index: 1, value: { type: "word", value: "foo*bar" } } } },
  { name: "double quotes with variable", input: { command: "echo \"value is $VAR\"" }, expected: { tokenAt: { index: 1, value: { type: "word", value: "value is $VAR" } } } }
];

runTestSuite("Edge Case Redirect Tests", edgeCaseRedirectTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  if (tc.expected.redirectCount) {
    const cmds = extractCommands(tokens);
    assertEquals(cmds[0].redirects.length, tc.expected.redirectCount);
  }
  if (tc.expected.tokenAt) {
    assertEquals(tokens[tc.expected.tokenAt.index], tc.expected.tokenAt.value);
  }
});

// Edge cases - policy evaluation
const edgeCasePolicyTests: TestCase[] = [
  { name: "command with flags before args", input: { command: "ls -la /tmp" }, expected: { action: "allow" } },
  { name: "subshell with redirection", input: { command: "(echo hi) > file.txt" }, expected: { hasSubshell: true } }
];

runTestSuite("Edge Case Policy Tests", edgeCasePolicyTests, (tc) => {
  if (tc.expected.action) {
    const result = evaluateCommand(tc.input.command as string, productionPolicy);
    assertEquals(result.action, tc.expected.action);
  }
  if (tc.expected.hasSubshell) {
    const tokens = tokenize(tc.input.command as string);
    const cmds = extractCommands(tokens);
    assertTrue(cmds.some((c) => c.source === "subshell"));
  }
});

// Double dash edge cases
const doubleDashTests: TestCase[] = [
  { name: "double dash as word", input: { command: "grep -- -v file" }, expected: { length: 4, tokenAt: { index: 1, value: { type: "word", value: "--" } } } },
  { name: "double dash command does not recurse for grep", input: { command: "grep -- -v file" }, expected: { commandCount: 1, firstCommand: { name: "grep", fullText: "grep -- -v file" } } },
  { name: "double dash with sudo extraction", input: { command: "sudo -- ls -la" }, expected: { commandCount: 2, commands: [{ name: "sudo" }, { name: "ls" }] } },
  { 
    name: "double dash in bash -c wrapper", 
    setup: () => ({ policy: { allow: [{ match: "grep", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands }),
    input: { command: "bash -c 'grep -- -v file'" }, 
    expected: { action: "allow" } 
  }
];

runTestSuite("Double Dash Tests", doubleDashTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  if (tc.expected.length) assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.tokenAt) assertEquals(tokens[tc.expected.tokenAt.index], tc.expected.tokenAt.value);
  if (tc.expected.commandCount) {
    const cmds = extractCommands(tokens);
    assertEquals(cmds.length, tc.expected.commandCount);
    if (tc.expected.firstCommand) {
      if (tc.expected.firstCommand.name) assertEquals(cmds[0].name, tc.expected.firstCommand.name);
      if (tc.expected.firstCommand.fullText) assertEquals(cmds[0].fullText, tc.expected.firstCommand.fullText);
    }
    if (tc.expected.commands) {
      for (let i = 0; i < tc.expected.commands.length; i++) {
        if (tc.expected.commands[i].name) assertEquals(cmds[i].name, tc.expected.commands[i].name);
      }
    }
  }
  if (tc.expected.action) {
    const { policy } = tc.setup ? tc.setup() : { policy: productionPolicy };
    const result = evaluateCommand(tc.input.command as string, policy as PolicyCommands);
    assertEquals(result.action, tc.expected.action);
  }
});

// Multiline string handling
const multilineTests: TestCase[] = [
  { name: "multiline in double quotes", input: { command: "echo \"line1\nline2\"" }, expected: { length: 2, hasNewline: true } },
  { name: "multiline in single quotes", input: { command: "echo 'line1\nline2'" }, expected: { length: 2, hasNewline: true } }
];

runTestSuite("Multiline Tests", multilineTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  assertEquals(tokens.length, tc.expected.length);
  if (tc.expected.hasNewline) {
    assertTrue((tokens[1] as { value: string }).value.includes("\n"));
  }
});

// Heredoc with shell script content
const heredocContentTests: TestCase[] = [
  {
    name: "heredoc with shell commands - text content not executed",
    setup: () => ({
      policy: { allow: [{ match: "cat", mode: "prefix" }], ask: [], deny: [{ match: "rm -rf /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "cat <<EOF\nrm -rf /\nEOF" },
    expected: { action: "allow" }
  },
  {
    name: "bash heredoc - heredoc body not extracted",
    setup: () => ({
      policy: { allow: [{ match: "bash", mode: "prefix" }], ask: [], deny: [{ match: "rm", mode: "prefix" }] } as PolicyCommands
    }),
    input: { command: "bash <<EOF\nrm -rf /\nEOF" },
    expected: { action: "allow" }
  }
];

runTestSuite("Heredoc Content Tests", heredocContentTests, (tc) => {
  const { policy } = tc.setup();
  const result = evaluateCommand(tc.input.command as string, policy);
  assertEquals(result.action, tc.expected.action);
});

// Complex stderr/stdout redirect combinations
const fdRedirectTests: TestCase[] = [
  { name: "redirect stdout to file stderr to stdout", input: { command: "cmd > file 2>&1" }, expected: { redirectCount: 2, firstOp: ">", secondOp: "2>&" } },
  { name: "redirect stderr first then stdout", input: { command: "cmd 2>&1 > file" }, expected: { redirectCount: 2, firstOp: "2>&", secondOp: ">" } },
  { name: "explicit fd redirects", input: { command: "cmd 1>out.txt 2>err.txt" }, expected: { redirectCount: 2, firstOp: "1>", secondOp: "2>" } },
  { name: "append with stderr merge", input: { command: "cmd >> file 2>&1" }, expected: { redirectCount: 2, firstOp: ">>", secondOp: "2>&" } },
  { name: "bash ampersand redirect syntax", input: { command: "cmd &> file" }, expected: { minRedirects: 1 } }
];

runTestSuite("FD Redirect Tests", fdRedirectTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const redirects = tokens.filter((t) => t.type === "redirect");
  if (tc.expected.redirectCount) assertEquals(redirects.length, tc.expected.redirectCount);
  if (tc.expected.minRedirects) assertTrue(redirects.length >= tc.expected.minRedirects);
  if (tc.expected.firstOp) assertEquals((redirects[0] as { op: string }).op, tc.expected.firstOp);
  if (tc.expected.secondOp) assertEquals((redirects[1] as { op: string }).op, tc.expected.secondOp);
});

// Deny keyword in various contexts
const denyContextTests: TestCase[] = [
  {
    name: "deny substring in heredoc body - not extracted",
    setup: () => ({
      policy: { allow: [{ match: "bash", mode: "prefix" }], ask: [], deny: [{ match: "rm -rf /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "bash <<EOF\necho hi\nrm -rf /\nEOF" },
    expected: { action: "allow" }
  },
  {
    name: "deny keyword in quoted string still matches substring",
    setup: () => ({
      policy: { allow: [], ask: [], deny: [{ match: "rm -rf /", mode: "substring" }] } as PolicyCommands
    }),
    input: { command: "echo 'rm -rf /'" },
    expected: { action: "deny" }
  }
];

runTestSuite("Deny Context Tests", denyContextTests, (tc) => {
  const { policy } = tc.setup();
  const result = evaluateCommand(tc.input.command as string, policy);
  assertEquals(result.action, tc.expected.action);
});

// Complex multiline command
const multilineCommandTests: TestCase[] = [
  { name: "complex multiline command", input: { command: "echo 'start' && \\\n  echo 'middle' && \\\n  echo 'end'" }, expected: { commandCount: 3, allEcho: true } }
];

runTestSuite("Multiline Command Tests", multilineCommandTests, (tc) => {
  const tokens = tokenize(tc.input.command as string);
  const cmds = extractCommands(tokens);
  assertEquals(cmds.length, tc.expected.commandCount);
  if (tc.expected.allEcho) {
    assertEquals(cmds.every((c) => c.name === "echo"), true);
  }
});

// SECURITY: Double-dash wrapper bypass attempts
const securityTests: TestCase[] = [
  {
    name: "passthrough wrapper skips -- and extracts inner command",
    setup: () => ({
      tokens: tokenize("time -- echo hi"),
      policy: { allow: [{ match: "time", mode: "prefix" }], ask: [], deny: [{ match: "echo", mode: "prefix" }] } as PolicyCommands
    }),
    input: { command: "time -- echo hi" },
    expected: { commandCount: 2, commands: [{ name: "time", source: "direct" }, { name: "echo", source: "wrapper-arg" }], action: "deny", reasonIncludes: "echo" }
  },
  {
    name: "bash -c -- 'cmd' treats -- as command string",
    input: { command: "bash -c -- 'rm -rf /'" },
    expected: { commandCount: 2, commands: [{ name: "bash" }, { name: "--" }] }
  },
  {
    name: "bash -c without -- extracts correctly",
    input: { command: "bash -c 'rm -rf /'" },
    expected: { commandCount: 2, commands: [{ name: "bash" }, { name: "rm" }] }
  },
  {
    name: "env -- VAR=val cmd extracts actual utility",
    input: { command: "env -- VAR=val cmd" },
    expected: { commandCount: 2, commands: [{ name: "env" }, { name: "cmd" }] }
  }
];

runTestSuite("Security Tests", securityTests, (tc) => {
  const { tokens, policy } = tc.setup ? tc.setup() : { tokens: undefined, policy: undefined };
  const testTokens = tokens || tokenize(tc.input.command as string);
  const cmds = extractCommands(testTokens);
  if (tc.expected.commandCount) assertEquals(cmds.length, tc.expected.commandCount);
  if (tc.expected.commands) {
    for (let i = 0; i < tc.expected.commands.length; i++) {
      if (tc.expected.commands[i].name) assertEquals(cmds[i].name, tc.expected.commands[i].name);
      if (tc.expected.commands[i].source) assertEquals(cmds[i].source, tc.expected.commands[i].source);
    }
  }
  if (tc.expected.action && policy) {
    const result = evaluateCommand(tc.input.command as string, policy);
    assertEquals(result.action, tc.expected.action);
    if (tc.expected.reasonIncludes) {
      assertTrue(result.reason?.includes(tc.expected.reasonIncludes as string));
    }
  }
});

// ==================== EXTENSIBILITY TESTS ====================

const extensibilityTests: TestCase[] = [
  {
    name: "custom wrapper via config without code change",
    setup: () => ({
      customRules: buildWrapperRuleMap([{ name: "custom-wrapper", kind: "utility-operand" } as WrapperRuleConfig])
    }),
    input: { command: "custom-wrapper -- cmd arg" },
    expected: { commandCount: 2, firstCommand: { name: "custom-wrapper" }, secondCommand: { name: "cmd", source: "wrapper-arg" } }
  },
  {
    name: "evaluateCommand with custom wrapper rules",
    setup: () => ({
      customRules: buildWrapperRuleMap([{ name: "mycmd", kind: "utility-operand" } as WrapperRuleConfig]),
      policy: { allow: [{ match: "inner", mode: "prefix" }], ask: [], deny: [] } as PolicyCommands
    }),
    input: { command: "mycmd inner" },
    expected: { action: "allow" }
  }
];

runTestSuite("Extensibility Tests", extensibilityTests, (tc) => {
  const { customRules, policy } = tc.setup();
  if (tc.expected.commandCount) {
    const tokens = tokenize(tc.input.command as string);
    const cmds = extractCommands(tokens, "direct", customRules);
    assertEquals(cmds.length, tc.expected.commandCount);
    if (tc.expected.firstCommand) {
      if (tc.expected.firstCommand.name) assertEquals(cmds[0].name, tc.expected.firstCommand.name);
      if (tc.expected.firstCommand.source) assertEquals(cmds[0].source, tc.expected.firstCommand.source);
    }
    if (tc.expected.secondCommand) {
      if (tc.expected.secondCommand.name) assertEquals(cmds[1].name, tc.expected.secondCommand.name);
      if (tc.expected.secondCommand.source) assertEquals(cmds[1].source, tc.expected.secondCommand.source);
    }
  }
  if (tc.expected.action) {
    const result = evaluateCommand(tc.input.command as string, policy, customRules);
    assertEquals(result.action, tc.expected.action);
  }
});

// ==================== END EDGE CASE & REGRESSION TESTS ====================

// Run all tests and print summary
function runAllTests(): void {
  console.log("=== Shell Policy Engine Test Suite ===\n");

  // Execute all registered test suites
  executeTestSuites();

  console.log("\n=== Summary ===");
  console.log(`${stats.passed} passed, ${stats.failed} failed`);
  if (stats.failures.length > 0) {
    console.log("\nFailures:");
    stats.failures.forEach((f) => console.log(`  - ${f}`));
    process.exit(1);
  }
}

runAllTests();
