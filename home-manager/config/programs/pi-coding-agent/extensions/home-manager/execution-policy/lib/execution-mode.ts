import type { ExtensionContext } from "@mariozechner/pi-coding-agent";

export interface ExecutionModeState {
  mode: string;
  policyOverride?: { write?: string[]; edit?: string[] };
}

const modeCache = new Map<string, ExecutionModeState>();

export function getExecutionMode(ctx: ExtensionContext): ExecutionModeState {
  const cacheKey = ctx.sessionManager.getSessionFile() ?? "ephemeral";
  const cached = modeCache.get(cacheKey);
  if (cached) return cached;

  let mode = "edit";
  let policyOverride: ExecutionModeState["policyOverride"];
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === "execution-mode") {
      const data = entry.data as ExecutionModeState;
      mode = data?.mode || "edit";
      policyOverride = data?.policyOverride;
    }
  }
  const result = { mode, policyOverride };
  modeCache.set(cacheKey, result);
  return result;
}

export function clearModeCache(cacheKey?: string | null) {
  if (cacheKey) {
    modeCache.delete(cacheKey);
  } else {
    modeCache.clear();
  }
}
