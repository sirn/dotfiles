import {
  keyHint,
  getLatestCompactionEntry,
  type ExtensionAPI,
  type ExtensionContext,
  type ExtensionCommandContext,
} from "@earendil-works/pi-coding-agent";
import { Container, Text, Box, Spacer } from "@earendil-works/pi-tui";
import * as fs from "node:fs";
import * as path from "node:path";
import {
  MODE_EDIT,
  getMode,
  setMode,
  EXECUTION_MODE_ENTRY,
} from "./lib/contract.js";
import { PLAN_DIR } from "./lib/paths.js";

export const MODE_PLAN = "plan";

function updatePlanModeStatus(ctx: ExtensionContext): void {
  ctx.ui.setStatus(
    "execution-mode",
    getMode(ctx) === MODE_PLAN ? "\uF4A0 plan mode" : undefined,
  );
}

type ModelSelection = { provider: string; modelId: string };

type PlanExecutionPendingData = {
  status?: "pending" | "processed";
  planContent?: string;
  modelSelection?: ModelSelection;
  userMessage?: string;
};

type PendingPlanExecution = {
  planContent: string;
  modelSelection?: ModelSelection;
  userMessage?: string;
};

const PLAN_MODE_CONTEXT = "plan-mode-context";
const PLAN_MODE_EXIT = "plan-mode-exit";
const PLAN_MODE_EXECUTE = "plan-mode-execute";

const PLAN_MODE_PROMPT = `<plan-mode>
Plan mode is currently ACTIVE:
- Produce an implementation/execution plan at {PLAN_PATH} using write/edit tool
- DO NOT make any other changes, only read-only exploration and planning
- Ask the user if the instruction is unclear or need decision before writing a plan
- Plan should only contains relevant information for implementation
- Assume future session will not have access to our conversation
- Summarize plan to user; don't assume user will read the full plan
</plan-mode>`;

const PLAN_MODE_EXIT_PROMPT = `<plan-mode>
Plan mode has been EXITED — you are now in normal mode.
- The earlier plan-mode instructions no longer apply; disregard them.
- Resume normal operation and make changes as the user directs.
</plan-mode>`;

const PLAN_MODE_EXECUTE_PROMPT = `<plan-mode>
Plan approved - execute the implementation plan:
- Execute the plan one step at a time until the end; verify each step
- If appropriate subagent exist, delegate plan execution to subagent 
- If a step fail verification, fix it before asking
- Report progress to the user at every step
- Run the verification checklist after all steps complete
</plan-mode>
<plan>
{PLAN_CONTENT}
</plan>
<user-message>
{USER_MESSAGE}
</user-message>`;

export default function (pi: ExtensionAPI) {
  // Recently-compacted detection

  /**
   * Returns true if the session context was recently compacted.
   * "Recently" means the latest compaction entry is the last entry on the
   * current branch, or there are only a few non-user entries between it and the leaf.
   */
  function isRecentlyCompacted(ctx: ExtensionContext): boolean {
    const branch = ctx.sessionManager.getBranch();
    const latestCompaction = getLatestCompactionEntry(branch);
    if (!latestCompaction) return false;

    // If the latest compaction is the very last entry, it's definitely recent
    const leafEntry = branch[branch.length - 1];
    if (leafEntry && leafEntry.id === latestCompaction.id) return true;

    // Also consider it recent if the compaction is within the last few entries
    // (there may be custom entries appended after compaction, e.g. execution-mode)
    const compactionIndex = branch.lastIndexOf(latestCompaction);
    if (compactionIndex >= 0 && branch.length - compactionIndex <= 3)
      return true;

    return false;
  }

  function lastInstructionMode(
    ctx: ExtensionContext,
  ): "plan" | "edit" | undefined {
    const branch = ctx.sessionManager.getBranch();
    const latestCompaction = getLatestCompactionEntry(branch);
    const lo = latestCompaction ? branch.lastIndexOf(latestCompaction) : -1;
    for (let i = branch.length - 1; i > lo; i--) {
      const ct = (branch[i] as { customType?: string }).customType;
      if (ct === PLAN_MODE_CONTEXT) return "plan";
      if (ct === PLAN_MODE_EXIT || ct === PLAN_MODE_EXECUTE) return "edit";
    }
    return undefined;
  }

  function getPendingPlanExecution(
    ctx: ExtensionContext,
  ): PendingPlanExecution | undefined {
    const entries = ctx.sessionManager.getEntries();
    for (let i = entries.length - 1; i >= 0; i--) {
      const entry = entries[i];
      if (
        entry.type === "custom" &&
        entry.customType === "plan-execution-pending"
      ) {
        const data = entry.data as PlanExecutionPendingData;
        if (data?.status === "processed") {
          return undefined;
        }
        if (
          data?.status === "pending" &&
          typeof data.planContent === "string"
        ) {
          const modelSelection =
            typeof data.modelSelection?.provider === "string" &&
            typeof data.modelSelection?.modelId === "string"
              ? data.modelSelection
              : undefined;
          return {
            planContent: data.planContent,
            modelSelection,
            userMessage:
              typeof data.userMessage === "string"
                ? data.userMessage
                : undefined,
          };
        }
      }
    }
    return undefined;
  }

  function movePlanToSession(
    projectDir: string,
    sessionFile: string | undefined,
    sourcePlanPath: string,
    planContent: string,
  ) {
    const targetPlanPath = getPlanPath(projectDir, sessionFile);
    fs.mkdirSync(path.dirname(targetPlanPath), { recursive: true });

    if (sourcePlanPath === targetPlanPath) {
      if (!fs.existsSync(targetPlanPath)) {
        fs.writeFileSync(targetPlanPath, planContent, "utf-8");
      }
      return;
    }

    if (fs.existsSync(sourcePlanPath)) {
      fs.renameSync(sourcePlanPath, targetPlanPath);
      return;
    }

    fs.writeFileSync(targetPlanPath, planContent, "utf-8");
  }

  function getPlanPath(
    projectDir: string,
    sessionFile: string | undefined,
  ): string {
    const normalized = path
      .resolve(projectDir)
      .replace(/^\//, "")
      .replace(/\//g, "-");
    const sessionId = sessionFile
      ? path.basename(sessionFile, ".jsonl")
      : "default";
    return path.join(PLAN_DIR, `--${normalized}--`, `${sessionId}.md`);
  }

  function ctxPlanPath(ctx: ExtensionContext): string {
    return getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
  }

  function requirePlanMode(ctx: ExtensionContext, message: string): boolean {
    if (getMode(ctx) !== MODE_PLAN) {
      ctx.ui.notify(message, "error");
      return false;
    }
    return true;
  }
  pi.registerMessageRenderer(
    PLAN_MODE_EXECUTE,
    (message: any, { expanded }: { expanded: boolean }, theme: any) => {
      const container = new Container();
      const box = new Box(1, 1, (s: string) => theme.bg("customMessageBg", s));
      box.addChild(
        new Text(theme.fg("success", theme.bold(" plan approved")), 0, 0),
      );
      box.addChild(new Spacer(1));
      if (expanded) {
        const text =
          typeof message.content === "string"
            ? message.content
            : "Plan accepted.";
        box.addChild(new Text(theme.fg("customMessageText", text), 0, 0));
      } else {
        const userInstruction =
          message.details?.userInstruction || "Plan accepted.";
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
    },
  );

  async function restorePendingModelSelection(
    ctx: ExtensionContext,
    modelSelection: ModelSelection | undefined,
  ): Promise<void> {
    if (!modelSelection) return;

    const model = ctx.modelRegistry.find(
      modelSelection.provider,
      modelSelection.modelId,
    );
    if (!model) {
      ctx.ui.notify(
        `New session created, but failed to find previous model ${modelSelection.provider}/${modelSelection.modelId}.`,
        "warning",
      );
      return;
    }

    const restored = await pi.setModel(model);
    if (!restored) {
      ctx.ui.notify(
        `New session created, but failed to restore model ${modelSelection.provider}/${modelSelection.modelId}.`,
        "warning",
      );
    }
  }

  function sendExecutionMessage(planContent: string, userMessage?: string) {
    const message = userMessage
      ? userMessage.trim()
      : "No additional user message. Proceed according to the plan.";
    pi.sendMessage(
      {
        customType: PLAN_MODE_EXECUTE,
        content: PLAN_MODE_EXECUTE_PROMPT.replaceAll(
          "{PLAN_CONTENT}",
          planContent,
        ).replaceAll("{USER_MESSAGE}", message),
        display: true,
        details: { userInstruction: userMessage?.trim() || undefined },
      },
      { triggerTurn: true },
    );
  }

  async function handlePlanEnter(
    args: string,
    ctx: ExtensionContext,
  ): Promise<void> {
    const planPath = ctxPlanPath(ctx);
    setMode(pi, MODE_PLAN, { write: [planPath], edit: [planPath] });
    updatePlanModeStatus(ctx);
    fs.mkdirSync(path.dirname(planPath), { recursive: true });

    // Treat whitespace-only args as no args (matches original /plan behavior where
    // Pi passes undefined for empty, but unified dispatch passes raw string)
    if (args?.trim()) {
      pi.sendUserMessage(args);
    } else {
      const hasExistingPlan = fs.existsSync(planPath);
      ctx.ui.notify(
        hasExistingPlan
          ? "Plan mode active. Continue refining your plan."
          : "Plan mode active. Type your request to create a plan.",
        "info",
      );
    }
  }

  async function handlePlanAccept(
    args: string,
    ctx: ExtensionCommandContext,
  ): Promise<void> {
    if (!requirePlanMode(ctx, "No active plan found. Use /plan first.")) return;

    const planPath = ctxPlanPath(ctx);

    if (!fs.existsSync(planPath)) {
      ctx.ui.notify(`Plan file not found: ${planPath}`, "error");
      return;
    }

    const stat = fs.statSync(planPath);
    if (stat.size < 50) {
      ctx.ui.notify(
        "Plan file is too small or empty. Please write a detailed plan first.",
        "error",
      );
      return;
    }

    const recentlyCompacted = isRecentlyCompacted(ctx);

    const choice = recentlyCompacted
      ? await ctx.ui.select("Accept Plan?", [
          "Accept plan",
          "Accept plan and clear context",
          "Accept plan and compact",
          "Cancel",
        ])
      : await ctx.ui.select("Accept Plan?", [
          "Accept plan and compact",
          "Accept plan and clear context",
          "Accept plan",
          "Cancel",
        ]);

    if (choice === "Cancel" || choice === undefined) {
      ctx.ui.notify("Accept cancelled. Continue refining the plan.", "info");
      return;
    }

    setMode(pi, MODE_EDIT);
    updatePlanModeStatus(ctx);

    const planContent = fs.readFileSync(planPath, "utf-8");

    if (choice === "Accept plan and clear context") {
      ctx.ui.notify(
        "Plan accepted! Creating new session for fresh execution...",
        "success",
      );

      const previousModelSelection = ctx.model
        ? { provider: ctx.model.provider, modelId: ctx.model.id }
        : undefined;
      const parentSession = ctx.sessionManager.getSessionFile();
      const result = await ctx.newSession({
        parentSession,
        setup: async (sessionManager) => {
          movePlanToSession(
            ctx.cwd,
            sessionManager.getSessionFile(),
            planPath,
            planContent,
          );
          sessionManager.appendCustomEntry("plan-execution-pending", {
            status: "pending",
            planContent,
            modelSelection: previousModelSelection,
            userMessage: args || undefined,
          });
          sessionManager.appendCustomEntry(EXECUTION_MODE_ENTRY, {
            mode: MODE_EDIT,
          });
        },
      });

      if (result.cancelled) {
        ctx.ui.notify("Session creation cancelled.", "info");
        return;
      }

      ctx.ui.notify(
        "New session created. Starting plan execution...",
        "success",
      );
    } else if (choice === "Accept plan and compact") {
      if (recentlyCompacted) {
        ctx.ui.notify(
          "Context was recently compacted. Running compaction again...",
          "info",
        );
      } else {
        ctx.ui.notify(
          "Plan accepted! Compacting context for execution...",
          "success",
        );
      }
      ctx.compact({
        customInstructions:
          "User has accepted the implementation plan. Summarize the current conversation in a short, concise text focusing on the context needed for plan execution.",
        onComplete: () => {
          ctx.ui.notify("Context compacted. Ready for execution.", "success");
          sendExecutionMessage(planContent, args);
        },
      });
    } else if (choice === "Accept plan") {
      ctx.ui.notify("Plan accepted! Ready for execution.", "success");
      sendExecutionMessage(planContent, args);
    }
  }

  async function handlePlanShow(ctx: ExtensionContext): Promise<void> {
    const mode = getMode(ctx);
    const planPath = ctxPlanPath(ctx);
    if (mode !== MODE_PLAN || !fs.existsSync(planPath)) {
      ctx.ui.notify("No plan found. Use /plan to create one.", "error");
      return;
    }

    const content = fs.readFileSync(planPath, "utf-8");

    const edited = await ctx.ui.editor("Plan", content);

    if (edited && edited !== content) {
      fs.writeFileSync(planPath, edited, "utf-8");
      ctx.ui.notify("Plan updated manually.", "success");
    }
  }

  async function handlePlanCancel(ctx: ExtensionContext): Promise<void> {
    if (!requirePlanMode(ctx, "No active plan found.")) return;

    const planPath = ctxPlanPath(ctx);
    const choice = await ctx.ui.select("Cancel Plan?", [
      "Leave plan mode",
      "Leave plan mode and clear plan file",
      "Cancel",
    ]);

    if (choice === "Cancel" || choice === undefined) {
      ctx.ui.notify("Cancel aborted.", "info");
      return;
    }

    setMode(pi, MODE_EDIT);
    updatePlanModeStatus(ctx);

    if (choice === "Leave plan mode and clear plan file") {
      try {
        if (fs.existsSync(planPath)) {
          fs.unlinkSync(planPath);
          ctx.ui.notify("Plan mode cancelled. Plan file deleted.", "success");
        } else {
          ctx.ui.notify(
            "Plan mode cancelled. Plan file already removed.",
            "success",
          );
        }
      } catch (error) {
        ctx.ui.notify(
          `Plan cancelled but failed to delete file: ${error}`,
          "warning",
        );
      }
    } else {
      ctx.ui.notify("Plan mode cancelled. Back to normal mode.", "success");
    }
  }

  // Registration

  pi.registerCommand("plan", {
    description: "Plan mode: enter, accept, show, or cancel",
    getArgumentCompletions: (prefix: string) => {
      const token = prefix.trimStart();
      if (token.includes(" ")) return null;
      const subcommands = [
        {
          value: "accept",
          label: "accept",
          description: "Accept plan and trigger execution",
        },
        {
          value: "show",
          label: "show",
          description: "Display and edit the plan",
        },
        { value: "cancel", label: "cancel", description: "Cancel plan mode" },
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
        case "accept":
          return handlePlanAccept(rest, ctx as ExtensionCommandContext);
        case "show":
          return handlePlanShow(ctx);
        case "cancel":
          return handlePlanCancel(ctx);
        default:
          return handlePlanEnter(raw, ctx);
      }
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    updatePlanModeStatus(ctx);

    const pendingPlanExecution = getPendingPlanExecution(ctx);
    if (!pendingPlanExecution) return;

    await restorePendingModelSelection(
      ctx,
      pendingPlanExecution.modelSelection,
    );
    pi.appendEntry("plan-execution-pending", { status: "processed" });
    sendExecutionMessage(
      pendingPlanExecution.planContent,
      pendingPlanExecution.userMessage,
    );
  });

  pi.on("turn_end", async (_event, ctx) => {
    updatePlanModeStatus(ctx);
  });

  pi.on("before_agent_start", async (_event, ctx) => {
    const mode = getMode(ctx);
    const last = lastInstructionMode(ctx);

    if (mode === MODE_PLAN) {
      if (last === "plan") return;
      return {
        message: {
          customType: PLAN_MODE_CONTEXT,
          content: PLAN_MODE_PROMPT.replaceAll("{PLAN_PATH}", ctxPlanPath(ctx)),
          display: false,
        },
      };
    }

    if (last === "plan") {
      return {
        message: {
          customType: PLAN_MODE_EXIT,
          content: PLAN_MODE_EXIT_PROMPT,
          display: false,
        },
      };
    }
    return;
  });
}
