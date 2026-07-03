/**
 * Goal mode contract: types, state management, and detection helpers.
 *
 * This module is the single source of truth for goal-state shape, validation,
 * and status labelling. It also provides completion-detection helpers used by
 * the extension's agent_end hook to decide whether a continuation turn
 * represents natural completion (the agent declared the objective satisfied)
 * or a stall (the agent produced no tool calls and no completion signal).
 *
 * Design principles (aligned with Codex's approach):
 * - The harness trusts the model's judgment for *when* to stop.
 * - PRIMARY completion signal: the agent calls the `complete_goal` model
 *   tool to mark the objective achieved (Codex-style tool-based completion).
 * - FALLBACK completion signal: the agent's natural-language declaration,
 *   detected by regex, as a safety net for models that miss the tool.
 * - Stall detection (no tool calls + no completion signal) is a safety net,
 *   not a completion mechanism.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

// Constants

/** Custom-entry type used to persist goal state on the session branch. */
export const GOAL_STATE_ENTRY = "goal-state";

/** Custom-entry type for execution-mode (duplicated from plan-mode contract). */
export const EXECUTION_MODE_ENTRY = "execution-mode";
export const MODE_PLAN = "plan";

// Types

export type GoalStatus =
  | "active" // Goal is being actively driven.
  | "paused" // User paused auto-continuation.
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
 * Regex patterns that indicate the agent believes the overall objective is
 * complete. These are intentionally specific to avoid false positives from
 * sub-task completion language like "step 1 is done".
 *
 * The continuation prompt instructs the agent to say "The objective has been
 * completed" when done, so we look for close variants of that phrase.
 *
 * Pattern groups:
 *   (?:has been |is |was |has )?  — optional auxiliary verb
 *   (?:now )?                    — optional "now" ("the objective is now complete")
 *   (?:completed?|achieved|...)   — completion verb ("complete" or "completed")
 */
const COMPLETION_PATTERNS: readonly RegExp[] = [
  // "The objective has been completed" / "objective is now complete" / etc.
  /\bobjective\s+(?:has\s+been\s+|is\s+|was\s+|has\s+)?(?:now\s+)?(?:completed?|achieved|finished|done|satisfied|fulfilled)\b/i,
  // "The goal has been completed" / "goal is now complete" / etc.
  /\bgoal\s+(?:has\s+been\s+|is\s+|was\s+|has\s+)?(?:now\s+)?(?:completed?|achieved|finished|done|satisfied|fulfilled)\b/i,
  // "All requirements have been met" / "all tasks are now complete" / etc.
  /\ball\s+(?:requirements|tasks|steps|tests)\s+(?:have\s+been\s+|are\s+|were\s+)?(?:now\s+)?(?:met|satisfied|completed?|fulfilled|done|passing)\b/i,
  // "The task is complete" (broader)
  /\bthe\s+task\s+(?:has\s+been\s+|is\s+|was\s+)?(?:now\s+)?(?:completed?|achieved|finished|done)\b/i,
  // First-person: "I have completed the objective" / "I've finished the goal"
  /\bI(?:\s+have\s+|'ve\s+)(?:successfully\s+)?(?:completed?|achieved|finished|done)\s+(?:the\s+)?(?:objective|goal|task)\b/i,
];

/**
 * Extract the plain-text content of the last assistant message from a run's
 * message array. Handles both string content and structured content arrays
 * (pi's message format uses `part.type === "text"` for text blocks).
 */
export function extractLastAssistantText(messages: unknown[]): string {
  for (let i = messages.length - 1; i >= 0; i--) {
    const msg = messages[i] as { role?: string; content?: unknown };
    if (msg.role !== "assistant") continue;

    if (typeof msg.content === "string") return msg.content;
    if (Array.isArray(msg.content)) {
      return msg.content
        .filter((part: unknown) => (part as { type?: string }).type === "text")
        .map((part: unknown) => (part as { text?: string }).text ?? "")
        .join("");
    }
    return "";
  }
  return "";
}

/**
 * Check whether the last assistant message in a run declares the objective
 * as complete. Returns true if any completion pattern matches.
 *
 * This is used by the agent_end hook to auto-complete a goal when the agent
 * naturally finishes, rather than treating it as a stall.
 */
export function detectCompletion(messages: unknown[]): boolean {
  const text = extractLastAssistantText(messages);
  if (!text.trim()) return false;
  return COMPLETION_PATTERNS.some((re) => re.test(text));
}

/**
 * Detect whether ANY assistant message in a run made tool calls.
 *
 * A continuation that made tool calls earlier in the run but ends with
 * a text-only message should NOT be classified as stalled — the agent did
 * real work, it just chose to summarize at the end. Only a run with zero
 * tool calls across all assistant messages is a true stall.
 *
 * pi's message format uses `part.type === "toolCall"` (not "tool_use").
 */
export function runHadToolCalls(messages: unknown[]): boolean {
  for (const msg of messages as { role?: string; content?: unknown }[]) {
    if (msg.role === "assistant" && Array.isArray(msg.content)) {
      if (
        msg.content.some(
          (part: unknown) => (part as { type?: string }).type === "toolCall",
        )
      ) {
        return true;
      }
    }
  }
  return false;
}

/**
 * Detect whether ANY assistant message in a run called the `complete_goal`
 * tool. This is the PRIMARY completion signal: when the agent marks the goal
 * achieved via the model tool, the run is complete regardless of any other
 * text or tool activity. Mirrors the structure of `runHadToolCalls`.
 */
export function runCalledCompleteGoal(messages: unknown[]): boolean {
  for (const msg of messages as { role?: string; content?: unknown }[]) {
    if (msg.role === "assistant" && Array.isArray(msg.content)) {
      if (
        msg.content.some(
          (part: unknown) =>
            (part as { type?: string }).type === "toolCall" &&
            (part as { name?: string }).name === "complete_goal",
        )
      ) {
        return true;
      }
    }
  }
  return false;
}

/**
 * Outcome of analyzing a continuation turn for stall vs completion.
 *
 * - `"continue"`: The turn made tool calls and did not declare completion;
 *   the goal loop should continue.
 * - `"complete"`: The agent declared the objective satisfied; the goal
 *   should be auto-completed.
 * - `"stalled"`: The turn made no tool calls and declared no completion;
 *   the agent has stalled and the loop should stop.
 */
export type ContinuationOutcome = "continue" | "complete" | "stalled";

/**
 * Classify a self-triggered continuation turn. This consolidates the
 * two-tier detection logic so the agent_end hook stays readable and the
 * decision is fully testable without a running session.
 */
export function classifyContinuation(messages: unknown[]): ContinuationOutcome {
  // PRIMARY: the agent called the `complete_goal` tool to mark the objective
  // achieved. This takes priority over everything else.
  if (runCalledCompleteGoal(messages)) return "complete";
  // FALLBACK: the agent declared completion in natural language but did not
  // call the tool. Kept as a safety net for models that miss the tool.
  if (detectCompletion(messages)) return "complete";
  if (!runHadToolCalls(messages)) return "stalled";
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

/** Maximum objective length in characters (aligned with Codex). */
export const MAX_OBJECTIVE_CHARS = 4000;

/** Validate an objective string: non-empty and within the length limit. */
export function validateObjective(text: string): string | null {
  const trimmed = text.trim();
  if (!trimmed) return "Objective must not be empty.";
  // Count by Unicode code points (Array.from iterates code points, not UTF-16
  // units), matching Codex's `value.chars().count()`.
  const charCount = Array.from(trimmed).length;
  if (charCount > MAX_OBJECTIVE_CHARS) {
    return `Objective must be at most ${MAX_OBJECTIVE_CHARS} characters (got ${charCount}).`;
  }
  return null;
}

/**
 * Escape XML special characters in objective text before inserting it into
 * `<untrusted_objective>` tags. This prevents prompt injection via crafted
 * objectives that break out of the tag (aligned with Codex's escape_xml_text).
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

  let mode = "edit";
  for (const entry of ctx.sessionManager.getEntries()) {
    if (entry.type === "custom" && entry.customType === EXECUTION_MODE_ENTRY) {
      const data = entry.data as { mode?: string } | undefined;
      if (data?.mode) mode = data.mode;
    }
  }
  return mode === MODE_PLAN;
}
