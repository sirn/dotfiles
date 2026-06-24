/**
 * Tests for buildPathDiffHint
 * Run with: nix run nixpkgs#tsx -- vendor/extensions/shell-policy/tests/path-hint.test.ts
 */

import { buildPathDiffHint } from "../lib/path-hint.ts";

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

console.log("\n=== Path Diff Hint Tests ===");

test("single allowed, last segment differs", () => {
  assertEquals(
    buildPathDiffHint("/foo/bar/baz", ["/foo/bar/hoge"]),
    `section 3, "baz" -> "hoge"`,
  );
});

test("single allowed, middle segment differs", () => {
  assertEquals(
    buildPathDiffHint("/foo/bar/baz/foo/bar", ["/foo/bar/bax/foo/bar"]),
    `section 3, "baz" -> "bax"`,
  );
});

test("single allowed, no common prefix", () => {
  assertEquals(
    buildPathDiffHint("/hoge/hoge/hoge", ["/foo/bar/baz"]),
    "doesn't match allowed paths",
  );
});

test("multiple allowed, closest match wins", () => {
  assertEquals(
    buildPathDiffHint("/a/b/c", ["/x/y/z", "/a/b/d"]),
    `section 3, "c" -> "d"`,
  );
});

test("multiple allowed, closest (no tie)", () => {
  assertEquals(
    buildPathDiffHint(
      "/home/sirn/.pi/agent/plans/--project-a--/other.md",
      [
        "/home/sirn/.pi/agent/plans/--project-a--/default.md",
        "/home/sirn/.pi/agent/plans/--project-b--/default.md",
      ],
    ),
    `section 7, "other.md" -> "default.md"`,
  );
});

test("multiple allowed, tie same range (2-way combined)", () => {
  assertEquals(
    buildPathDiffHint(
      "/home/sirn/.pi/agent/plans/--project-c--/default.md",
      [
        "/home/sirn/.pi/agent/plans/--project-a--/default.md",
        "/home/sirn/.pi/agent/plans/--project-b--/default.md",
      ],
    ),
    `section 6, "--project-c--" -> either "--project-a--" or "--project-b--"`,
  );
});

test("multiple allowed, tie same range (3-way combined)", () => {
  assertEquals(
    buildPathDiffHint(
      "/home/sirn/.pi/agent/plans/--project-c--/default.md",
      [
        "/home/sirn/.pi/agent/plans/--project-a--/default.md",
        "/home/sirn/.pi/agent/plans/--project-b--/default.md",
        "/home/sirn/.pi/agent/plans/--project-d--/default.md",
      ],
    ),
    `section 6, "--project-c--" -> either "--project-a--" or "--project-b--" or "--project-d--"`,
  );
});

test("multiple allowed, tie different ranges (fallback first)", () => {
  assertEquals(
    buildPathDiffHint(
      "/home/sirn/.pi/agent/plans/--project-c--/other.md",
      [
        "/home/sirn/.pi/agent/plans/--project-a--/other.md",
        "/home/sirn/.pi/agent/plans/--project-b--/default.md",
      ],
    ),
    `section 6, "--project-c--" -> "--project-a--"`,
  );
});

test("multiple allowed, none close", () => {
  assertEquals(
    buildPathDiffHint("/tmp/whatever.md", ["/a/b/c", "/x/y/z"]),
    "doesn't match allowed paths",
  );
});

test("length mismatch: requested longer", () => {
  assertEquals(
    buildPathDiffHint("/foo/bar/baz", ["/foo/bar"]),
    `section 3, "baz" -> (path ends here)`,
  );
});

test("length mismatch: allowed longer", () => {
  assertEquals(
    buildPathDiffHint("/foo/bar", ["/foo/bar/baz"]),
    `section 3, (path ends here) -> "baz"`,
  );
});

test("common prefix 0 unrelated paths", () => {
  assertEquals(
    buildPathDiffHint("/etc/nixos/foo.nix", [
      "/home/sirn/.pi/agent/plans/default.md",
    ]),
    "doesn't match allowed paths",
  );
});

test("tilde literal treated as relative (no common prefix)", () => {
  assertEquals(
    buildPathDiffHint("~/foo", ["/home/sirn/bar"]),
    "doesn't match allowed paths",
  );
});

test("exact match returns null", () => {
  assertEquals(buildPathDiffHint("/foo/bar/baz", ["/foo/bar/baz"]), null);
});

test("undefined requested returns null", () => {
  assertEquals(buildPathDiffHint(undefined, ["/foo"]), null);
});

test("empty allowed array returns null", () => {
  assertEquals(buildPathDiffHint("/foo", []), null);
});

// Tests run inline above; print summary
console.log("\n=== Summary ===");
console.log(`${stats.passed} passed, ${stats.failed} failed`);
if (stats.failures.length > 0) {
  console.log("\nFailures:");
  stats.failures.forEach((f) => console.log(`  - ${f}`));
  process.exit(1);
}
