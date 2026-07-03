/**
 * Minimal, dependency-free caching helper for the goal-mode extension.
 *
 * Invalidation uses Option B: stat (mtime) AND a content hash that is
 * recomputed on every freshness check, so edits the stat did not surface
 * (same-length swaps, coarse mtime) still invalidate the cache.
 *
 * The cache is process-local (module-level Map).
 */

import * as fs from "node:fs/promises";

export interface StatInfo {
  mtimeMs: number;
  size: number;
}

export interface CacheEntry<T> {
  stat: StatInfo;
  hash: string;
  value: T;
  loadedAt: number;
}

export interface CacheOptions {
  maxAgeMs?: number;
}

const fileCache = new Map<string, CacheEntry<unknown>>();

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
