import type { ExtensionContext } from "@mariozechner/pi-coding-agent";

export interface ExecutionModeState {
  mode: string;
  policyOverride?: { write?: string[]; edit?: string[] };
}

export function getExecutionMode(ctx: ExtensionContext): ExecutionModeState {
  let mode = "edit";
  let policyOverride: ExecutionModeState["policyOverride"];
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === "execution-mode") {
      const data = entry.data as ExecutionModeState;
      mode = data?.mode || "edit";
      policyOverride = data?.policyOverride;
    }
  }
  return { mode, policyOverride };
}
