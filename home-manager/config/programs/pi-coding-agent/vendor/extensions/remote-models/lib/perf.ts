/**
 * Lightweight performance measurement hook.
 *
 * Active only when `PI_EXTENSION_PERF=1` is set, so it is silent in normal
 * production use. Wrap hot paths with `measure()` to log elapsed time.
 */

const PERF_ENABLED =
  process.env.PI_EXTENSION_PERF === "1" ||
  process.env.PI_EXTENSION_PERF === "true";

export function isPerfEnabled(): boolean {
  return PERF_ENABLED;
}

export async function measure<T>(
  label: string,
  fn: () => T | Promise<T>,
): Promise<T> {
  if (!PERF_ENABLED) return fn();
  const start = performance.now();
  try {
    return await fn();
  } finally {
    const elapsed = (performance.now() - start).toFixed(2);
    console.warn(`[pi-ext-perf] ${label}: ${elapsed}ms`);
  }
}
