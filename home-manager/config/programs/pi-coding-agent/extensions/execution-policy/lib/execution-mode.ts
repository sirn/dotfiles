import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

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

type ModeRegistration = {
  mode: string;
  getPrompt: (ctx: ExtensionContext) => string;
  widget?: {
    widgetId: string;
    label: string;
  };
};

const modeRegistrations = new Map<string, ModeRegistration>();

export function registerMode(config: ModeRegistration): void {
  modeRegistrations.set(config.mode, config);
}

// Mode Change Hook

type ModeChangeHook = (
  ctx: ExtensionContext,
  mode: string,
  modes: string[],
) => void;
let modeChangeHook: ModeChangeHook | undefined;

/**
 * Register a callback fired after mode state changes.
 * Called from updateModeWidgets, which is invoked on session_start,
 * mode commands, and turn_end.
 */
export function setModeChangeHook(hook: ModeChangeHook): void {
  modeChangeHook = hook;
}

/** Internal: fire the mode change hook. */
export function fireModeChangeHook(
  ctx: ExtensionContext,
  mode: string,
  modes: string[],
): void {
  modeChangeHook?.(ctx, mode, modes);
}

export function setupModePromptInjection(pi: ExtensionAPI): void {
  pi.on("before_agent_start", async (_event, ctx) => {
    const currentMode = getMode(ctx);
    if (currentMode === MODE_EDIT) return;

    const reg = modeRegistrations.get(currentMode);
    if (reg) {
      return {
        message: {
          customType: `${currentMode}-mode-context`,
          content: reg.getPrompt(ctx),
          display: false,
        },
      };
    }
  });
}

export function updateModeWidgets(ctx: ExtensionContext): void {
  const mode = getMode(ctx);
  let activeModeLabel: string | undefined;
  for (const [modeName, reg] of modeRegistrations) {
    if (!reg.widget) continue;
    // Clear widgets — mode info now shown via session footer status
    ctx.ui.setWidget(reg.widget.widgetId, undefined);
    if (modeName === mode) {
      activeModeLabel = reg.widget.label;
    }
  }
  ctx.ui.setStatus("execution-mode", activeModeLabel);
  fireModeChangeHook(ctx, mode, getExecutionMode(ctx).modes);
}
