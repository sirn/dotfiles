// Contract shared with shell-policy/lib/execution-mode.ts — keep
// EXECUTION_MODE_ENTRY, MODE_EDIT, MODE_YOLO, modeLabel, and policyOverride
// shape in sync across both extensions.
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

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

export function getMode(ctx: ExtensionContext): string {
  const envModes = (process.env.PI_EXECUTION_MODE ?? "")
    .split(",")
    .map((m) => m.trim())
    .filter(Boolean);
  if (envModes.length > 0) return envModes[envModes.length - 1];
  let mode = MODE_EDIT;
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === EXECUTION_MODE_ENTRY) {
      const data = entry.data as { mode?: string } | undefined;
      if (data?.mode) mode = data.mode;
    }
  }
  return mode;
}

export function setMode(
  pi: ExtensionAPI,
  mode: string,
  policyOverride?: { write?: string[]; edit?: string[] },
) {
  pi.appendEntry(EXECUTION_MODE_ENTRY, { mode, policyOverride });
}
