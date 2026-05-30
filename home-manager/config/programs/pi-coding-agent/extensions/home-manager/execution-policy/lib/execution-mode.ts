import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

export const MODE_DELEGATE = "delegate";
export const MODE_PLAN = "plan";
export const MODE_EDIT = "edit";
export const EXECUTION_MODE_ENTRY = "execution-mode";

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

export function sessionCacheKey(ctx: ExtensionContext): string {
  return ctx.sessionManager.getSessionFile() ?? "ephemeral";
}

export function getMode(ctx: ExtensionContext): string {
  return getExecutionMode(ctx).mode;
}

export function setMode(
  pi: ExtensionAPI,
  ctx: ExtensionContext,
  mode: string,
  policyOverride?: ExecutionModeState["policyOverride"],
) {
  const sessionId = sessionCacheKey(ctx);
  clearModeCache(sessionId);
  pi.appendEntry(EXECUTION_MODE_ENTRY, { mode, policyOverride });
}

export function getExecutionMode(ctx: ExtensionContext): ExecutionModeState {
  const cacheKey = sessionCacheKey(ctx);
  const cached = modeCache.get(cacheKey);
  if (cached) return cached;

  const envModes = parseExecutionModeStack(process.env.PI_EXECUTION_MODE);
  if (envModes.length > 0) {
    const result = { mode: envModes[envModes.length - 1], modes: envModes };
    modeCache.set(cacheKey, result);
    return result;
  }

  let mode = MODE_EDIT;
  let policyOverride: ExecutionModeState["policyOverride"];
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === EXECUTION_MODE_ENTRY) {
      const data = entry.data as Partial<ExecutionModeState>;
      mode = data?.mode || MODE_EDIT;
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
