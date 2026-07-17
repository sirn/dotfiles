// Contract shared with plan-mode/lib/contract.ts — keep
// EXECUTION_MODE_ENTRY, MODE_EDIT, MODE_YOLO, modeLabel, and
// policyOverride shape in sync across both extensions.
import type { ExtensionContext } from "@earendil-works/pi-coding-agent";

export const MODE_EDIT = "edit";
export const MODE_PLAN = "plan";
export const MODE_YOLO = "yolo";
export const EXECUTION_MODE_ENTRY = "execution-mode";

export function modeLabel(mode: string): string | undefined {
  switch (mode) {
    case MODE_PLAN:
      return "\uF4A0 plan mode";
    case MODE_YOLO:
      return "\ueb44 yolo mode";
    default:
      return undefined;
  }
}

export interface ExecutionModeState {
  mode: string;
  modes: string[];
  policyOverride?: { write?: string[]; edit?: string[] };
}

export function parseExecutionModeStack(raw: string | undefined): string[] {
  return (raw ?? "")
    .split(",")
    .map((mode) => mode.trim())
    .filter(Boolean);
}

export function getExecutionMode(ctx: ExtensionContext): ExecutionModeState {
  const envModes = parseExecutionModeStack(process.env.PI_EXECUTION_MODE);
  if (envModes.length > 0) {
    const result = { mode: envModes[envModes.length - 1], modes: envModes };
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
  return result;
}
