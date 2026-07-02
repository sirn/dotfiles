import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

export const GOAL_STATE_ENTRY = "goal-state";

export type GoalStatus =
  | "active"
  | "paused"
  | "complete"
  | "budget-limited"
  | "cleared";

export interface GoalBudget {
  maxTurns: number; // Infinity = no turn limit
  maxCost: number; // Infinity = no cost limit
}

export interface GoalState {
  objective: string;
  status: GoalStatus;
  budget: GoalBudget;
  budgetReason?: string; // set when status becomes "budget-limited"
}

// By default, goals have no budget limit. Users can set one with
// `/goal budget <turns|cost> <value>`.
export const DEFAULT_BUDGET: GoalBudget = {
  maxTurns: Infinity,
  maxCost: Infinity,
};

export function getGoalState(ctx: ExtensionContext): GoalState | null {
  let state: GoalState | null = null;
  // Goals are thread-scoped: only visible on the branch where they were set.
  for (const entry of ctx.sessionManager.getBranch()) {
    if (entry.type === "custom" && entry.customType === GOAL_STATE_ENTRY) {
      const data = entry.data as Partial<GoalState> | undefined;
      if (!data?.objective || !data?.status || !data?.budget) continue;
      const status = data.status as GoalStatus;
      if (
        status !== "active" &&
        status !== "paused" &&
        status !== "complete" &&
        status !== "budget-limited" &&
        status !== "cleared"
      ) {
        continue;
      }
      const maxTurns = data.budget.maxTurns ?? DEFAULT_BUDGET.maxTurns;
      const maxCost = data.budget.maxCost ?? DEFAULT_BUDGET.maxCost;
      // Infinity is valid (no limit); finite values must be positive.
      if (
        !(maxTurns === Infinity || (Number.isFinite(maxTurns) && maxTurns > 0))
      ) {
        continue;
      }
      if (
        !(maxCost === Infinity || (Number.isFinite(maxCost) && maxCost > 0))
      ) {
        continue;
      }
      const budgetReason =
        typeof data.budgetReason === "string" ? data.budgetReason : undefined;
      state = {
        objective: data.objective,
        status,
        budget: { maxTurns, maxCost },
        budgetReason,
      };
    }
  }
  return state;
}

export function setGoalState(pi: ExtensionAPI, state: GoalState): void {
  pi.appendEntry(GOAL_STATE_ENTRY, state);
}

export function goalStatusLabel(state: GoalState | null): string | undefined {
  if (!state) return undefined;
  switch (state.status) {
    case "active":
      return "\uF4DE goal: active";
    case "paused":
      return "\uF04C goal: paused";
    case "complete":
      return "\uF00C goal: complete";
    case "budget-limited":
      return `\uF421 goal: ${state.budgetReason ?? "budget-limited"}`;
    default:
      return undefined;
  }
}

// Read-only execution-mode detection duplicated from plan-mode's contract
// (the repo convention is to duplicate this contract per extension, as
// shell-policy does). Goal mode never writes execution-mode entries.
export const EXECUTION_MODE_ENTRY = "execution-mode";
export const MODE_PLAN = "plan";

export function isPlanMode(ctx: ExtensionContext): boolean {
  const envModes = (process.env.PI_EXECUTION_MODE ?? "")
    .split(",")
    .map((m) => m.trim())
    .filter(Boolean);
  if (envModes.length > 0) return envModes[envModes.length - 1] === MODE_PLAN;
  let mode = "edit";
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === EXECUTION_MODE_ENTRY) {
      const data = entry.data as { mode?: string } | undefined;
      if (data?.mode) mode = data.mode;
    }
  }
  return mode === MODE_PLAN;
}
