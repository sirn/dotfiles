/**
 * Minimal, dependency-free caching helpers for the subagent extension.
 *
 * Invalidation uses Option B: stat (mtime + size) AND a content hash that is
 * recomputed on every freshness check. This catches edits that leave mtime/size
 * unchanged (e.g. same-length content swaps on filesystems with coarse mtime
 * granularity) and — for directories — edits to existing member files, since a
 * directory's own mtime only changes when entries are added or removed.
 *
 * Caches are process-local (module-level Maps).
 */

import * as fs from "node:fs/promises";
import * as path from "node:path";
import type * as fsSync from "node:fs";

export interface StatInfo {
  mtimeMs: number;
  size: number;
}

export interface CacheEntry<T> {
  stat: StatInfo;
  /** Content hash stamped when the value was loaded. */
  hash: string;
  value: T;
  loadedAt: number;
}

export interface CacheOptions {
  /** Maximum age of a cached entry regardless of stat/hash (ms). */
  maxAgeMs?: number;
}

const fileCache = new Map<string, CacheEntry<unknown>>();
const dirCache = new Map<string, CacheEntry<unknown>>();

async function statOrNull(p: string): Promise<StatInfo | null> {
  try {
    const st = await fs.stat(p);
    return { mtimeMs: st.mtimeMs, size: st.size };
  } catch {
    return null;
  }
}

// FNV-1a 32-bit; identity only, not cryptographic.
function hashString(input: string): string {
  let h = 0x811c9dc5;
  for (let i = 0; i < input.length; i++) {
    h ^= input.charCodeAt(i);
    h = Math.imul(h, 0x01000193);
  }
  return (h >>> 0).toString(16).padStart(8, "0");
}

/**
 * Read a file through the cache. `loader` transforms the raw file content into
 * a value; the value is reused only while stat (mtime + size) AND a freshly
 * recomputed content hash all match (or it expires by `maxAgeMs`).
 *
 * Returns `null` when the file does not exist.
 */
export async function memoizeByStat<T>(
  filePath: string,
  loader: (content: string, stat: StatInfo) => T | Promise<T>,
  options: CacheOptions = {},
): Promise<T | null> {
  const stat = await statOrNull(filePath);
  if (!stat) {
    fileCache.delete(filePath);
    return null;
  }

  const existing = fileCache.get(filePath) as CacheEntry<T> | undefined;
  const maxAge = options.maxAgeMs;
  if (existing) {
    const notExpired =
      maxAge === undefined || Date.now() - existing.loadedAt <= maxAge;
    // Recompute the content hash fresh so edits the stat did not surface
    // (same-length swaps, coarse mtime) still invalidate the cache.
    if (notExpired && existing.stat.mtimeMs === stat.mtimeMs) {
      const content = await fs.readFile(filePath, "utf-8");
      if (hashString(content) === existing.hash) {
        return existing.value;
      }
    }
  }

  const content = await fs.readFile(filePath, "utf-8");
  const value = await loader(content, stat);
  fileCache.set(filePath, {
    stat,
    hash: hashString(content),
    value,
    loadedAt: Date.now(),
  });
  return value;
}

/**
 * Scan a directory through the cache. `loader` receives the `Dirent[]` and
 * stat. Recomputes a content-derived signature on every call from each member
 * file's stat, so editing an existing `.md` invalidates the cached parse even
 * though the directory's own mtime does not change. Returns `null` when the
 * directory is missing.
 */
export async function memoizeDirectoryByStat<T>(
  dirPath: string,
  loader: (entries: fsSync.Dirent[], stat: StatInfo) => T | Promise<T>,
  options: CacheOptions = {},
): Promise<T | null> {
  const stat = await statOrNull(dirPath);
  if (!stat) {
    dirCache.delete(dirPath);
    return null;
  }

  const entries = await fs.readdir(dirPath, { withFileTypes: true });
  // Build a fresh signature from member file stats so edits to existing files
  // (which don't change the directory mtime) are detected.
  const memberStats = await Promise.all(
    entries.map(async (e) => {
      if (!e.isFile() && !e.isSymbolicLink()) return `${e.name}:dir`;
      try {
        const st = await fs.stat(path.join(dirPath, e.name));
        return `${e.name}:${st.mtimeMs}:${st.size}`;
      } catch {
        return `${e.name}:gone`;
      }
    }),
  );
  const sig = hashString(memberStats.join("|"));

  const existing = dirCache.get(dirPath) as CacheEntry<T> | undefined;
  const maxAge = options.maxAgeMs;
  if (existing) {
    const notExpired =
      maxAge === undefined || Date.now() - existing.loadedAt <= maxAge;
    if (notExpired && existing.hash === sig) {
      return existing.value;
    }
  }

  const value = await loader(entries, stat);
  dirCache.set(dirPath, {
    stat,
    hash: sig,
    value,
    loadedAt: Date.now(),
  });
  return value;
}

/** Drop a single directory's cached value (if any). */
export function invalidateDirectoryCache(dirPath: string): void {
  dirCache.delete(dirPath);
}
