import path from "node:path";

function splitSegments(p: string): string[] {
  const resolved = path.resolve(p);
  const parts = resolved.split(path.sep);
  if (parts.length > 0 && parts[0] === "") parts.shift(); // drop POSIX root ""
  return parts;
}

function commonPrefixLen(req: string[], allow: string[]): number {
  const n = Math.min(req.length, allow.length);
  let i = 0;
  while (i < n && req[i] === allow[i]) i++;
  return i;
}

interface DiffResult {
  commonPrefix: number;
  firstDiff: number; // -1 when paths are identical
  lastDiff: number; // -1 when paths are identical
  identical: boolean;
}

function computeDiff(requested: string, allowed: string): DiffResult {
  const req = splitSegments(requested);
  const allow = splitSegments(allowed);
  const cp = commonPrefixLen(req, allow);
  const maxLen = Math.max(req.length, allow.length);
  let firstDiff = -1, lastDiff = -1;
  for (let i = 0; i < maxLen; i++) {
    const r = i < req.length ? req[i] : undefined;
    const a = i < allow.length ? allow[i] : undefined;
    if (r !== a) {
      if (firstDiff === -1) firstDiff = i;
      lastDiff = i;
    }
  }
  if (firstDiff === -1) {
    return { commonPrefix: req.length, firstDiff: -1, lastDiff: -1, identical: true };
  }
  return { commonPrefix: cp, firstDiff, lastDiff, identical: false };
}

function formatSubpath(segments: string[], firstDiff: number, lastDiff: number): string {
  const slash = firstDiff === 0 ? "/" : "";
  const joined = slash + segments.slice(firstDiff, lastDiff + 1).join("/");
  return joined === "" ? "(path ends here)" : `"${joined}"`;
}

function formatRange(firstDiff: number, lastDiff: number): string {
  const start = firstDiff + 1, end = lastDiff + 1;
  return start === end ? `section ${start}` : `section ${start}-${end}`;
}

export function buildPathDiffHint(
  requestedPath: string | undefined,
  allowedPaths: string[] | undefined,
): string | null {
  if (!requestedPath || !allowedPaths || allowedPaths.length === 0) return null;

  const diffs = allowedPaths.map((allowed) => ({
    allowed,
    ...computeDiff(requestedPath, allowed),
  }));

  const maxCp = Math.max(...diffs.map((d) => d.commonPrefix));
  if (maxCp === 0) return "doesn't match allowed paths";

  // Tied = max common prefix, excluding exact matches (shouldn't be blocked).
  const tied = diffs.filter((d) => d.commonPrefix === maxCp && !d.identical);
  if (tied.length === 0) return null;

  const req = splitSegments(requestedPath);

  // Single closest path: straight diff.
  if (tied.length === 1) {
    const d = tied[0];
    return `${formatRange(d.firstDiff, d.lastDiff)}, ${formatSubpath(req, d.firstDiff, d.lastDiff)} -> ${formatSubpath(splitSegments(d.allowed), d.firstDiff, d.lastDiff)}`;
  }

  // Multiple tied paths. If they share the same diff range, combine
  // the allowed subpaths: `... -> either "A" or "B"`.
  const sameRange = tied.every((d) =>
    d.firstDiff === tied[0].firstDiff && d.lastDiff === tied[0].lastDiff,
  );
  if (sameRange) {
    const allowParts = tied.map((d) =>
      formatSubpath(splitSegments(d.allowed), d.firstDiff, d.lastDiff),
    );
    return `${formatRange(tied[0].firstDiff, tied[0].lastDiff)}, ${formatSubpath(req, tied[0].firstDiff, tied[0].lastDiff)} -> either ${allowParts.join(" or ")}`;
  }

  // Tied but different ranges — not structurally comparable; use the first.
  const d = tied[0];
  return `${formatRange(d.firstDiff, d.lastDiff)}, ${formatSubpath(req, d.firstDiff, d.lastDiff)} -> ${formatSubpath(splitSegments(d.allowed), d.firstDiff, d.lastDiff)}`;
}
