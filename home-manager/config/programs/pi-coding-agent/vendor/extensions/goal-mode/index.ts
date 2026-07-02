import {
  keyHint,
  getLatestCompactionEntry,
  type ExtensionAPI,
  type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { Container, Text, Box, Spacer } from "@earendil-works/pi-tui";
import * as path from "node:path";
import {
  GOAL_STATE_ENTRY,
  DEFAULT_BUDGET,
  getGoalState,
  setGoalState,
  goalStatusLabel,
  isPlanMode,
  type GoalBudget,
} from "./lib/contract.js";
import { PROMPTS_DIR } from "./lib/paths.js";
import { memoizeByStat } from "./lib/cache.js";

const GOAL_CONTEXT_TYPE = "goal-context";
const GOAL_BUDGET_REACHED_TYPE = "goal-budget-reached";
const GOAL_CONTINUATION_TYPE = "goal-continuation";
const GOAL_SET_TYPE = "goal-set";

// Inline fallbacks used only when the external prompt files are missing;
// the long real instruction bodies live in vendor/prompts/goal-mode/*.md.
const GOAL_ACTIVE_FALLBACK = `<goal-mode>A goal is currently active: {OBJECTIVE}</goal-mode>`;
const GOAL_CONTINUE_FALLBACK = `<goal-continuation>The goal is still active: {OBJECTIVE}</goal-continuation>`;
const GOAL_BUDGET_FALLBACK = `<goal-budget-reached>The goal budget has been reached: {OBJECTIVE}</goal-budget-reached>`;

interface GoalPrompts {
  active: string;
  continue: string;
  budgetReached: string;
}

async function loadGoalPrompts(): Promise<GoalPrompts> {
  const [active, cont, budget] = await Promise.all([
    loadPrompt("goal-active.md"),
    loadPrompt("goal-continue.md"),
    loadPrompt("goal-budget-reached.md"),
  ]);
  return {
    active: active ?? GOAL_ACTIVE_FALLBACK,
    continue: cont ?? GOAL_CONTINUE_FALLBACK,
    budgetReached: budget ?? GOAL_BUDGET_FALLBACK,
  };
}

function loadPrompt(name: string): Promise<string | null> {
  const p = path.join(PROMPTS_DIR, name);
  return memoizeByStat(p, (content) => content);
}

export default function (pi: ExtensionAPI) {
  // Tracks whether the current agent run was triggered by our continuation
  // sendMessage, so agent_end can apply the anti-spin check only to
  // self-triggered turns. Cleared at the top of before_agent_start (each
  // new user-driven run clears it) and by every command handler that
  // changes goal state.
  let pendingContinuationTurn = false;

  // Cached combined branch scan: recently-compacted detection and
  // last-goal-injection dedup both walk getBranch(); computing them together
  // in one pass avoids two full scans per turn. Cached by branch length +
  // leaf id so repeated calls within the same turn are free.
  let branchScanCache:
    | {
        key: string;
        recentlyCompacted: boolean;
        lastGoalInjected: boolean;
      }
    | undefined;

  function scanBranch(ctx: ExtensionContext): {
    recentlyCompacted: boolean;
    lastGoalInjected: boolean;
  } {
    const branch = ctx.sessionManager.getBranch();
    const leafEntry = branch[branch.length - 1];
    const key = `${branch.length}:${leafEntry?.id ?? ""}`;
    if (branchScanCache && branchScanCache.key === key) {
      return branchScanCache;
    }

    const latestCompaction = getLatestCompactionEntry(branch);
    let recentlyCompacted = false;
    if (latestCompaction) {
      if (leafEntry && leafEntry.id === latestCompaction.id) {
        recentlyCompacted = true;
      } else {
        const compactionIndex = branch.lastIndexOf(latestCompaction);
        recentlyCompacted =
          compactionIndex >= 0 && branch.length - compactionIndex <= 3;
      }
    }

    // Find the last goal-state entry; only goal-context entries AFTER it
    // (and after the last compaction, whichever is later) count as injected.
    // After replacing/resuming a goal, old context entries must not cause
    // dedup to skip re-injection for the new goal.
    let lastGoalStateIdx = -1;
    for (let i = branch.length - 1; i >= 0; i--) {
      if (
        branch[i].type === "custom" &&
        (branch[i] as { customType?: string }).customType === GOAL_STATE_ENTRY
      ) {
        lastGoalStateIdx = i;
        break;
      }
    }
    const lo = latestCompaction ? branch.lastIndexOf(latestCompaction) : -1;
    const scanStart = Math.max(lastGoalStateIdx, lo);
    let lastGoalInjected = false;
    for (let i = branch.length - 1; i > scanStart; i--) {
      const ct = (branch[i] as { customType?: string }).customType;
      if (ct === GOAL_CONTEXT_TYPE) {
        lastGoalInjected = true;
        break;
      }
    }

    branchScanCache = { key, recentlyCompacted, lastGoalInjected };
    return branchScanCache;
  }

  // Budget is derived from all assistant messages after the LAST goal-state
  // entry. When a goal is set or resumed, a new goal-state entry is appended,
  // so the budget window resets from that point.
  function deriveBudgetUsage(ctx: ExtensionContext): {
    turns: number;
    cost: number;
  } {
    const branch = ctx.sessionManager.getBranch() as Array<{
      type: string;
      customType?: string;
      message?: {
        role: string;
        usage?: { cost?: { total: number } };
      };
    }>;
    let startIndex = 0;
    for (let i = branch.length - 1; i >= 0; i--) {
      const entry = branch[i];
      if (entry.type === "custom" && entry.customType === GOAL_STATE_ENTRY) {
        startIndex = i + 1;
        break;
      }
    }
    let turns = 0;
    let cost = 0;
    for (let i = startIndex; i < branch.length; i++) {
      const entry = branch[i];
      if (entry.type === "message" && entry.message?.role === "assistant") {
        turns++;
        cost += entry.message.usage?.cost?.total ?? 0;
      }
    }
    return { turns, cost };
  }

  // Anti-spin: detect whether ANY assistant message in the run made tool
  // calls. A continuation that made tool calls earlier in the run but ends
  // with a text-only message should NOT be classified as stalled.
  // pi's message format uses part.type === "toolCall" (not "tool_use").
  function runHadToolCalls(messages: unknown[]): boolean {
    for (const msg of messages as {
      role?: string;
      content?: unknown;
    }[]) {
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

  // Format a budget limit for display: Infinity becomes "unlimited".
  function fmtLimit(n: number): string {
    return n === Infinity ? "unlimited" : String(n);
  }
  function fmtCost(n: number): string {
    return n === Infinity ? "unlimited" : `$${n.toFixed(2)}`;
  }

  function updateGoalStatus(ctx: ExtensionContext): void {
    ctx.ui.setStatus("goal-status", goalStatusLabel(getGoalState(ctx)));
  }

  // Command handlers

  async function handleGoalSet(
    objective: string,
    ctx: ExtensionContext,
  ): Promise<void> {
    const trimmed = objective.trim();
    if (!trimmed) {
      ctx.ui.notify("Usage: /goal <objective>", "error");
      return;
    }

    const existing = getGoalState(ctx);
    if (existing && existing.status !== "cleared") {
      const choice = await ctx.ui.select("A goal already exists. Replace it?", [
        "Replace goal",
        "Cancel",
      ]);
      if (choice !== "Replace goal") {
        ctx.ui.notify("Goal unchanged.", "info");
        return;
      }
    }

    setGoalState(pi, {
      objective: trimmed,
      status: "active",
      budget: { ...DEFAULT_BUDGET },
    });
    pendingContinuationTurn = false;
    updateGoalStatus(ctx);

    // Send a goal-set message that renders as a box (like plan-mode's
    // "plan approved") and triggers the first turn toward the objective.
    pi.sendMessage(
      {
        customType: GOAL_SET_TYPE,
        content: trimmed,
        display: true,
        details: { userInstruction: trimmed },
      },
      { triggerTurn: true },
    );
  }

  function handleGoalView(ctx: ExtensionContext): void {
    const state = getGoalState(ctx);
    if (!state || state.status === "cleared") {
      ctx.ui.notify("No active goal.", "info");
      return;
    }
    const usage = deriveBudgetUsage(ctx);
    ctx.ui.notify(
      `Objective: ${state.objective}\nStatus: ${state.status}\nBudget: ${usage.turns}/${fmtLimit(state.budget.maxTurns)} turns, $${usage.cost.toFixed(2)}/${fmtCost(state.budget.maxCost)}`,
      "info",
    );
  }

  function handleGoalPause(ctx: ExtensionContext): void {
    const state = getGoalState(ctx);
    if (!state || state.status === "cleared") {
      ctx.ui.notify("No active goal to pause.", "error");
      return;
    }
    if (state.status !== "active") {
      ctx.ui.notify(`Goal is already ${state.status}.`, "warning");
      return;
    }
    setGoalState(pi, { ...state, status: "paused" });
    pendingContinuationTurn = false;
    updateGoalStatus(ctx);
    ctx.ui.notify("Goal paused.", "info");
  }

  function handleGoalResume(ctx: ExtensionContext): void {
    const state = getGoalState(ctx);
    if (!state || state.status === "cleared") {
      ctx.ui.notify("No goal to resume.", "error");
      return;
    }
    if (state.status === "active") {
      ctx.ui.notify("Goal is already active.", "info");
      return;
    }
    // Resuming appends a fresh goal-state entry, which resets the budget
    // window (deriveBudgetUsage counts from the last goal-state entry).
    setGoalState(pi, {
      objective: state.objective,
      status: "active",
      budget: state.budget,
    });
    pendingContinuationTurn = false;
    updateGoalStatus(ctx);
    ctx.ui.notify("Goal resumed.", "success");
  }

  function handleGoalClear(ctx: ExtensionContext): void {
    const state = getGoalState(ctx);
    if (!state || state.status === "cleared") {
      ctx.ui.notify("No goal to clear.", "info");
      return;
    }
    setGoalState(pi, { ...state, status: "cleared" });
    pendingContinuationTurn = false;
    updateGoalStatus(ctx);
    ctx.ui.notify("Goal cleared.", "info");
  }

  function handleGoalComplete(ctx: ExtensionContext): void {
    const state = getGoalState(ctx);
    if (!state || state.status === "cleared") {
      ctx.ui.notify("No active goal to complete.", "error");
      return;
    }
    setGoalState(pi, { ...state, status: "complete" });
    pendingContinuationTurn = false;
    updateGoalStatus(ctx);
    ctx.ui.notify("Goal marked as complete.", "success");
  }

  function handleGoalBudget(args: string, ctx: ExtensionContext): void {
    const state = getGoalState(ctx);
    if (!state || state.status === "cleared") {
      ctx.ui.notify("No active goal to adjust.", "error");
      return;
    }
    const parts = args.trim().split(/\s+/).filter(Boolean);
    if (parts.length !== 2) {
      ctx.ui.notify("Usage: /goal budget <turns|cost> <value>", "error");
      return;
    }
    const [field, valueStr] = parts;
    let value: number;
    if (valueStr === "unlimited" || valueStr === "inf") {
      value = Infinity;
    } else {
      value = Number(valueStr);
      if (!Number.isFinite(value) || value <= 0) {
        ctx.ui.notify(
          "Budget value must be a positive number or 'unlimited'.",
          "error",
        );
        return;
      }
    }
    const budget: GoalBudget = { ...state.budget };
    if (field === "turns") {
      if (value !== Infinity && (!Number.isSafeInteger(value) || value < 1)) {
        ctx.ui.notify(
          "Turns budget must be an integer >= 1 or 'unlimited'.",
          "error",
        );
        return;
      }
      budget.maxTurns = value;
    } else if (field === "cost") {
      budget.maxCost = value;
    } else {
      ctx.ui.notify("Unknown budget field. Use 'turns' or 'cost'.", "error");
      return;
    }
    setGoalState(pi, { ...state, budget });
    pendingContinuationTurn = false;
    updateGoalStatus(ctx);
    ctx.ui.notify(
      `Budget updated: ${fmtLimit(budget.maxTurns)} turns, ${fmtCost(budget.maxCost)}.`,
      "success",
    );
  }

  // Command registration

  pi.registerCommand("goal", {
    description:
      "Goal mode: set, view, pause, resume, clear, complete, or adjust budget",
    getArgumentCompletions: (prefix: string) => {
      const token = prefix.trimStart();
      if (token.includes(" ")) return null;
      const subcommands = [
        {
          value: "pause",
          label: "pause",
          description: "Pause the active goal",
        },
        {
          value: "resume",
          label: "resume",
          description: "Resume a paused goal",
        },
        {
          value: "clear",
          label: "clear",
          description: "Remove the current goal",
        },
        {
          value: "complete",
          label: "complete",
          description: "Mark the goal as complete",
        },
        { value: "budget", label: "budget", description: "Adjust goal budget" },
      ];
      const filtered = subcommands.filter((s) => s.value.startsWith(token));
      return filtered.length > 0 ? filtered : null;
    },
    handler: async (args, ctx) => {
      const raw = args ?? "";
      const dispatch = raw.trimStart();
      const spaceIdx = dispatch.indexOf(" ");
      const first = spaceIdx === -1 ? dispatch : dispatch.slice(0, spaceIdx);
      const rest = spaceIdx === -1 ? "" : dispatch.slice(spaceIdx + 1);
      switch (first) {
        case "pause":
          return handleGoalPause(ctx);
        case "resume":
          return handleGoalResume(ctx);
        case "clear":
          return handleGoalClear(ctx);
        case "complete":
          return handleGoalComplete(ctx);
        case "budget":
          return handleGoalBudget(rest, ctx);
        default:
          if (!raw.trim()) return handleGoalView(ctx);
          return handleGoalSet(raw, ctx);
      }
    },
  });

  // Helper for the goal-box message renderers below: build the box with
  // a colored header, then render either the full content (expanded) or the
  // user instruction with an expand hint (collapsed). Mirrors plan-mode's
  // PLAN_MODE_EXECUTE renderer structure exactly.
  function makeGoalBoxRenderer(
    headerText: string,
    headerColor: string,
    fallbackText: string,
  ) {
    return (message: any, { expanded }: { expanded: boolean }, theme: any) => {
      const container = new Container();
      const box = new Box(1, 1, (s: string) => theme.bg("customMessageBg", s));
      box.addChild(
        new Text(theme.fg(headerColor, theme.bold(headerText)), 0, 0),
      );
      box.addChild(new Spacer(1));
      if (expanded) {
        const text =
          typeof message.content === "string" ? message.content : fallbackText;
        box.addChild(new Text(theme.fg("customMessageText", text), 0, 0));
      } else {
        const userInstruction =
          typeof message.details?.userInstruction === "string" &&
          message.details.userInstruction.trim()
            ? message.details.userInstruction
            : fallbackText;
        box.addChild(
          new Text(theme.fg("customMessageText", userInstruction), 0, 0),
        );
        box.addChild(new Spacer(1));
        box.addChild(
          new Text(
            `${theme.fg("muted", "(")}${keyHint("app.tools.expand", "to expand")}${theme.fg("muted", ")")}`,
            0,
            0,
          ),
        );
      }
      container.addChild(box);
      return {
        render: (width: number) => container.render(width),
        invalidate: () => container.invalidate(),
      };
    };
  }

  // Message renderer: render the goal-set message as a box (like plan-mode's
  // "plan approved" box) so the objective is visible in the conversation.
  pi.registerMessageRenderer(
    GOAL_SET_TYPE,
    makeGoalBoxRenderer("\uF4DE goal set", "success", "Goal set."),
  );

  // Message renderer: render continuation messages as a box. Collapsed
  // shows the user instruction; expanded shows the full continuation prompt.
  pi.registerMessageRenderer(
    GOAL_CONTINUATION_TYPE,
    makeGoalBoxRenderer(
      "\uF4DE goal continuation",
      "accent",
      "asking agent to continue the goal",
    ),
  );

  // Message renderer: render budget-reached messages as a box. Collapsed
  // shows the user instruction; expanded shows the full budget-reached prompt.
  pi.registerMessageRenderer(
    GOAL_BUDGET_REACHED_TYPE,
    makeGoalBoxRenderer(
      "\uF421 goal budget reached",
      "warning",
      "asking agent to summarize and stop",
    ),
  );

  // Hooks

  pi.on("session_start", async (_event, ctx) => {
    updateGoalStatus(ctx);
  });

  pi.on("turn_end", async (_event, ctx) => {
    updateGoalStatus(ctx);
  });

  pi.on("session_shutdown", async (_event, ctx) => {
    if (ctx.hasUI) ctx.ui.setStatus("goal-status", undefined);
  });

  pi.on("before_agent_start", async (_event, ctx) => {
    // A new user-driven run clears any pending auto-turn state.
    pendingContinuationTurn = false;

    const state = getGoalState(ctx);
    if (!state || state.status !== "active") return;

    const { lastGoalInjected, recentlyCompacted } = scanBranch(ctx);
    // Inject once per branch, and re-inject after compaction.
    if (lastGoalInjected && !recentlyCompacted) return;

    const prompts = await loadGoalPrompts();
    const usage = deriveBudgetUsage(ctx);
    const content = prompts.active
      .replaceAll("{OBJECTIVE}", state.objective)
      .replaceAll("{TURNS_USED}", String(usage.turns))
      .replaceAll("{MAX_TURNS}", fmtLimit(state.budget.maxTurns))
      .replaceAll("{COST_USED}", usage.cost.toFixed(2))
      .replaceAll("{MAX_COST}", fmtCost(state.budget.maxCost));

    return {
      message: {
        customType: GOAL_CONTEXT_TYPE,
        content,
        display: false,
      },
    };
  });

  pi.on("agent_end", async (event, ctx) => {
    // If the just-finished turn was a continuation we sent, read and clear
    // the flag. A new user-driven run also clears it in before_agent_start.
    const wasContinuationTurn = pendingContinuationTurn;
    pendingContinuationTurn = false;

    const state = getGoalState(ctx);
    if (!state || state.status !== "active") return;

    // Don't hijack plan mode; let plan-mode drive its own turns.
    if (isPlanMode(ctx)) return;

    // Anti-spin: if the just-finished continuation auto-turn made no tool
    // calls anywhere in the run, the agent has stalled — stop continuing.
    const messages = (event as { messages?: unknown[] }).messages ?? [];
    if (wasContinuationTurn && !runHadToolCalls(messages)) {
      ctx.ui.notify(
        "Goal continuation stopped: no tool calls in last turn.",
        "info",
      );
      return;
    }

    // Skip if compaction just happened; the post-compaction agent_end will
    // handle continuation once context settles.
    const { recentlyCompacted } = scanBranch(ctx);
    if (recentlyCompacted) return;

    // Skip if context is near the threshold; let smart-compact run first.
    const ctxUsage = ctx.getContextUsage();
    if (ctxUsage?.tokens != null && ctxUsage.contextWindow > 0) {
      if (ctxUsage.tokens > ctxUsage.contextWindow * 0.85) return;
    }

    const usage = deriveBudgetUsage(ctx);

    // Budget exhausted? (Infinity means no limit, so the comparison is false.)
    if (
      usage.turns >= state.budget.maxTurns ||
      usage.cost >= state.budget.maxCost
    ) {
      const reason =
        usage.turns >= state.budget.maxTurns
          ? `turn limit reached (${usage.turns}/${state.budget.maxTurns})`
          : `cost limit reached ($${usage.cost.toFixed(2)}/$${state.budget.maxCost.toFixed(2)})`;
      setGoalState(pi, {
        ...state,
        status: "budget-limited",
        budgetReason: reason,
      });
      updateGoalStatus(ctx);
      const prompts = await loadGoalPrompts();
      const content = prompts.budgetReached.replaceAll(
        "{OBJECTIVE}",
        state.objective,
      );
      pi.sendMessage(
        {
          customType: GOAL_BUDGET_REACHED_TYPE,
          content,
          display: true,
          details: { userInstruction: state.objective },
        },
        { triggerTurn: true },
      );
      return;
    }

    // Send continuation.
    const prompts = await loadGoalPrompts();
    const content = prompts.continue
      .replaceAll("{OBJECTIVE}", state.objective)
      .replaceAll(
        "{TURNS_REMAINING}",
        fmtLimit(state.budget.maxTurns - usage.turns),
      )
      .replaceAll(
        "{COST_REMAINING}",
        state.budget.maxCost === Infinity
          ? "unlimited"
          : `$${(state.budget.maxCost - usage.cost).toFixed(2)}`,
      );

    pendingContinuationTurn = true;
    pi.sendMessage(
      {
        customType: GOAL_CONTINUATION_TYPE,
        content,
        display: true,
        details: { userInstruction: state.objective },
      },
      { triggerTurn: true },
    );
  });
}
