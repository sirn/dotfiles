/**
 * Goal mode contract: types, state management, and detection helpers.
 *
 * This module is the single source of truth for goal-state shape, validation,
 * and status labeling. It also provides completion-detection and turn-error
 * helpers used by the extension's agent_end hook.
 *
 * Design principles:
 * - The harness trusts the model's judgment for *when* to stop.
 * - Completion signal: the agent calls the `update_goal` model tool to
 *   mark the objective achieved (tool-based completion). There is no
 *   regex fallback or stall detection — a continuation turn with no tool
 *   calls is trusted to continue.
 * - Turn errors (provider/transport failures) are mapped to "blocked" or
 *   "usage-limited" status.
 * - Budgets are an optional backstop; unlimited by default.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

// Constants

/** Custom-entry type used to persist goal state on the session branch. */
export const GOAL_STATE_ENTRY = "goal-state";

/**
 * Custom-entry type for execution-mode. Duplicated from plan-mode/lib/contract.ts
 * (and shell-policy/lib/execution-mode.ts) by convention — keep in sync across
 * all three. goal-mode reads this read-only to defer its hooks during plan mode;
 * it never writes execution-mode entries.
 */
export const EXECUTION_MODE_ENTRY = "execution-mode";
export const MODE_EDIT = "edit";
export const MODE_PLAN = "plan";

// Types

export type GoalStatus =
  | "active" // Goal is being actively driven.
  | "paused" // User paused auto-continuation.
  | "blocked" // Agent at a true impasse (3-strike); resumable like paused.
  | "usage-limited" // Provider rate/billing limit.
  | "complete" // Goal declared complete (by user or auto-detection).
  | "budget-limited" // Budget exhausted; agent asked to summarize.
  | "cleared"; // Goal removed; not shown in status bar.

export interface GoalBudget {
  /** Maximum number of assistant turns. Infinity = no limit. */
  maxTurns: number;
  /** Maximum cost in USD. Infinity = no limit. */
  maxCost: number;
}

export interface GoalState {
  objective: string;
  status: GoalStatus;
  budget: GoalBudget;
  /** Human-readable reason set when status becomes "budget-limited". */
  budgetReason?: string;
}

/** Default budget: unlimited turns and cost. */
export const DEFAULT_BUDGET: GoalBudget = {
  maxTurns: Infinity,
  maxCost: Infinity,
};

// Completion detection

/**
 * Detect whether ANY assistant message in a run called the `update_goal`
 * tool with status=`complete`. This is the PRIMARY completion signal: when
 * the agent marks the goal achieved via the model tool, the run is complete
 * regardless of any other text or tool activity.
 */
export function runCalledCompleteGoal(messages: unknown[]): boolean {
  for (const msg of messages as { role?: string; content?: unknown }[]) {
    if (msg.role === "assistant" && Array.isArray(msg.content)) {
      if (
        msg.content.some((part: unknown) => {
          const p = part as {
            type?: string;
            name?: string;
            input?: { status?: string };
          };
          return (
            p.type === "toolCall" &&
            p.name === "update_goal" &&
            p.input?.status === "complete"
          );
        })
      ) {
        return true;
      }
    }
  }
  return false;
}

/** Pattern matching usage/rate/billing limit error messages. */
const USAGE_LIMIT_PATTERN =
  /usage limit|rate limit|quota|billing|insufficient_quota|out of budget/i;

/**
 * Inspect the last assistant message in a run for a terminal error.
 * Pi surfaces provider/transport failures as a final AssistantMessage
 * with stopReason "error" or "aborted" and errorMessage populated.
 * Returns "usage-limited" if the error message indicates a usage/rate/
 * billing limit, otherwise "blocked". Returns null if no error is detected.
 */
export function detectTurnError(messages: unknown[]): GoalStatus | null {
  for (let i = messages.length - 1; i >= 0; i--) {
    const msg = messages[i] as {
      role?: string;
      stopReason?: string;
      errorMessage?: string;
    };
    if (msg.role !== "assistant") continue;
    if (msg.stopReason === "error" || msg.stopReason === "aborted") {
      return USAGE_LIMIT_PATTERN.test(msg.errorMessage ?? "")
        ? "usage-limited"
        : "blocked";
    }
    return null; // last assistant message is not an error
  }
  return null;
}

/**
 * Outcome of analyzing a continuation turn.
 *
 * - `"continue"`: The turn did not signal completion; the goal loop
 *   should continue.
 * - `"complete"`: The agent called update_goal with status=complete;
 *   the goal should be auto-completed.
 */
export type ContinuationOutcome = "continue" | "complete";

/**
 * Classify a self-triggered continuation turn. Returns "complete" if the
 * agent called update_goal with status=complete (the tool-based completion
 * signal), otherwise "continue". There is no stall detection: a
 * continuation with no tool calls is trusted to continue (gated solely by
 * status === "active").
 */
export function classifyContinuation(messages: unknown[]): ContinuationOutcome {
  if (runCalledCompleteGoal(messages)) return "complete";
  return "continue";
}

// Budget validation helpers

/**
 * Validate that a budget value is positive, finite (or Infinity), and for
 * turns specifically, a safe integer >= 1.
 */
export function isValidBudgetValue(
  value: number,
  field: "turns" | "cost",
): boolean {
  if (value === Infinity) return true;
  if (!Number.isFinite(value) || value <= 0) return false;
  if (field === "turns" && (!Number.isSafeInteger(value) || value < 1)) {
    return false;
  }
  return true;
}

// Objective validation and escaping

/** Maximum objective length in characters. */
export const MAX_OBJECTIVE_CHARS = 4000;

/** Validate an objective string: non-empty and within the length limit. */
export function validateObjective(text: string): string | null {
  const trimmed = text.trim();
  if (!trimmed) return "Objective must not be empty.";
  // Count by Unicode code points (Array.from iterates code points, not UTF-16
  // units).
  const charCount = Array.from(trimmed).length;
  if (charCount > MAX_OBJECTIVE_CHARS) {
    return `Objective must be at most ${MAX_OBJECTIVE_CHARS} characters (got ${charCount}).`;
  }
  return null;
}

/**
 * Escape XML special characters in objective text before inserting it into
 * `<untrusted_objective>` tags. This prevents prompt injection via crafted
 * objectives that break out of the tag.
 */
export function escapeXmlText(input: string): string {
  return input
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

// State management

/**
 * Read the current goal state from the session branch.
 *
 * Goals are thread-scoped: only visible on the branch where they were set.
 * The most recent valid goal-state entry wins.
 */
export function getGoalState(ctx: ExtensionContext): GoalState | null {
  let state: GoalState | null = null;
  for (const entry of ctx.sessionManager.getBranch()) {
    if (entry.type === "custom" && entry.customType === GOAL_STATE_ENTRY) {
      const data = entry.data as Partial<GoalState> | undefined;
      if (typeof data?.objective !== "string" || !data.objective) continue;
      if (!data?.status || !data?.budget) continue;

      const status = data.status as GoalStatus;
      if (
        status !== "active" &&
        status !== "paused" &&
        status !== "blocked" &&
        status !== "usage-limited" &&
        status !== "complete" &&
        status !== "budget-limited" &&
        status !== "cleared"
      ) {
        continue;
      }

      const maxTurns =
        typeof data.budget.maxTurns === "number"
          ? data.budget.maxTurns
          : DEFAULT_BUDGET.maxTurns;
      const maxCost =
        typeof data.budget.maxCost === "number"
          ? data.budget.maxCost
          : DEFAULT_BUDGET.maxCost;

      if (!isValidBudgetValue(maxTurns, "turns")) continue;
      if (!isValidBudgetValue(maxCost, "cost")) continue;

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

/** Append a goal-state entry to the session. */
export function setGoalState(pi: ExtensionAPI, state: GoalState): void {
  pi.appendEntry(GOAL_STATE_ENTRY, state);
}

/**
 * Return the status-bar label for a goal state, or undefined if no goal is
 * active (in which case the status slot is cleared).
 */
export function goalStatusLabel(state: GoalState | null): string | undefined {
  if (!state) return undefined;
  switch (state.status) {
    case "active":
      return "\uF4DE goal: active";
    case "paused":
      return "\uF04C goal: paused";
    case "blocked":
      return "\uF4E8 goal: blocked";
    case "usage-limited":
      return "\uF071 goal: usage-limited";
    case "complete":
      return "\uF00C goal: complete";
    case "budget-limited":
      return `\uF421 goal: ${state.budgetReason ?? "budget-limited"}`;
    default:
      return undefined;
  }
}

// Execution-mode detection (duplicated from plan-mode's contract)

/**
 * Read-only execution-mode detection. Goal mode never writes execution-mode
 * entries; this is used only to check whether plan mode is active so goal
 * mode can defer to it.
 */
export function isPlanMode(ctx: ExtensionContext): boolean {
  const envModes = (process.env.PI_EXECUTION_MODE ?? "")
    .split(",")
    .map((m) => m.trim())
    .filter(Boolean);
  if (envModes.length > 0) return envModes[envModes.length - 1] === MODE_PLAN;

  let mode = MODE_EDIT;
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === EXECUTION_MODE_ENTRY) {
      const data = entry.data as { mode?: string } | undefined;
      if (data?.mode) mode = data.mode;
    }
  }
  return mode === MODE_PLAN;
}
