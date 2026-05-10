import type { ExtensionContext } from "@mariozechner/pi-coding-agent";

export interface ExecutionModeState {
  mode: string;
  modes: string[];
  policyOverride?: { write?: string[]; edit?: string[] };
}

const modeCache = new Map<string, ExecutionModeState>();

export function parseExecutionModeStack(raw: string | undefined): string[] {
  return (raw ?? "")
    .split(",")
    .map((mode) => mode.trim())
    .filter(Boolean);
}

export function getExecutionMode(ctx: ExtensionContext): ExecutionModeState {
  const cacheKey = ctx.sessionManager.getSessionFile() ?? "ephemeral";
  const cached = modeCache.get(cacheKey);
  if (cached) return cached;

  const envModes = parseExecutionModeStack(process.env.PI_EXECUTION_MODE);
  if (envModes.length > 0) {
    const result = { mode: envModes[envModes.length - 1], modes: envModes };
    modeCache.set(cacheKey, result);
    return result;
  }

  let mode = "edit";
  let policyOverride: ExecutionModeState["policyOverride"];
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === "execution-mode") {
      const data = entry.data as Partial<ExecutionModeState>;
      mode = data?.mode || "edit";
      policyOverride = data?.policyOverride;
    }
  }
  const result = { mode, modes: [mode], policyOverride };
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
