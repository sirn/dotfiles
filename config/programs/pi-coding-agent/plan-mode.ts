import {
  keyHint,
  type ExtensionAPI,
  type ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { Container, Text, Box, Spacer } from "@mariozechner/pi-tui";

interface PlanState {
  phase: "idle" | "plan";
  approved: boolean;
}

export default function (pi: ExtensionAPI) {
  const PLAN_DIR = path.join(os.homedir(), ".pi/agent/plans");

  const stateMap = new Map<string, PlanState>();

  function getPlanPath(
    projectDir: string,
    sessionFile: string | undefined,
  ): string {
    const normalized = path.resolve(projectDir).replace(/^\//, "").replace(/\//g, "-");
    const sessionId = sessionFile
      ? path.basename(sessionFile, ".jsonl")
      : "default";
    return path.join(PLAN_DIR, `--${normalized}--`, `${sessionId}.md`);
  }

  function loadState(ctx: ExtensionContext): PlanState | null {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    if (stateMap.has(sessionId)) return stateMap.get(sessionId)!;

    let state: PlanState | null = null;
    for (const entry of ctx.sessionManager.getEntries()) {
      if (entry.type === "custom" && entry.customType === "plan-mode") {
        state = entry.data as PlanState;
      }
    }

    if (state) stateMap.set(sessionId, state);
    return state;
  }

  function saveState(ctx: ExtensionContext, state: PlanState) {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    stateMap.set(sessionId, state);
    pi.appendEntry("plan-mode", state);
  }

  function createMessageRenderer(
    header: string,
    colorKey: "accent" | "success",
    fallback: string,
  ) {
    return (message: any, { expanded }: { expanded: boolean }, theme: any) => {
      const container = new Container();

      // Box(1, 1) provides the colored padding top and bottom automatically
      const box = new Box(1, 1, (s: string) => theme.bg("customMessageBg", s));

      if (expanded) {
        box.addChild(new Text(theme.fg(colorKey, theme.bold(header)), 0, 0));
        box.addChild(new Spacer(1)); // colored empty line between header and body
        const text = typeof message.content === "string" ? message.content : fallback;
        box.addChild(new Text(text, 0, 0));
      } else {
        box.addChild(new Text(theme.fg(colorKey, theme.bold(header)) + theme.fg("muted", ` (${keyHint("expandTools", "to expand")})`), 0, 0));
      }

      container.addChild(box);

      return {
        render: (width: number) => container.render(width),
        invalidate: () => container.invalidate(),
      };
    };
  }

  pi.registerMessageRenderer("plan-mode-prompt", createMessageRenderer("󰏯 plan mode", "accent", "Plan requested."));
  pi.registerMessageRenderer("plan-mode-execute", createMessageRenderer("󰏫 plan approved", "success", "Plan accepted."));

  function sendExecutionMessage(planContent: string) {
    pi.sendMessage(
      {
        customType: "plan-mode-execute",
        content: `The plan has been approved. Execute the implementation plan step by step.

Rules:
- Execute ONE step at a time. Verify success before proceeding. Do NOT combine steps.
- If a step fails, analyze the error. If it fails twice, STOP and wait for user input.
- Keep changes minimal and idiomatic. Only modify in-scope files (§1).
- Skip trivial text verifications (e.g., checking if a string was added) unless doing mass/complex changes.
- Run the verification checklist (§6) after all steps complete.

Plan Content:
${planContent}`,
        display: true,
      },
      { triggerTurn: true },
    );
  }

  pi.registerCommand("plan", {
    description: "Create an implementation plan for review",
    handler: async (args, ctx) => {
      const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
      fs.mkdirSync(path.dirname(planPath), { recursive: true });

      saveState(ctx, { phase: "plan", approved: false });

      pi.sendMessage(
        {
          customType: "plan-mode-prompt",
          content: `Create a detailed implementation/execution plan based on the user instruction below:

- CRITICAL: Use ONLY read-only commands (ls, rg, cat, read) for context gathering. Do NOT execute changes.
- Read project README/config files first to understand conventions and tooling.
- **Use web search**. Load the relevant search skill and perform a search. Do not guess.
- Search current API/library documentation. Verify usage; do not assume. 
- Use the \`write\` tool to write the plan file. Do NOT use bash to write files (they'll be blocked).

Write the plan to:
${planPath}

## User instruction
${args || "The requested feature"}

## Plan structure

### 1. Overview and Scope
What needs to be built/fixed, why, and how success is measured. What is OUT of scope. Keep the solution minimal.

### 2. Context Summary
Document read-only exploration discoveries: key files, existing patterns, dependencies, and non-obvious details.
Example: "\`src/auth.ts\` exports \`createSession(userId)\`; sessions stored in Redis with 24h TTL"

### 3. Implementation Steps
Ordered, atomic, verifiable steps. Each needs: a clear goal, specific files/changes, and a concrete success criterion (e.g., test command, linter run).
Example: "Step 1: Add Session type to types.ts — Success: tsc --noEmit passes"

### 4. Detailed Code Changes
Describe non-trivial changes in detail to prevent guesswork. Show before/after with ±5 lines of context if helpful.

### 5. Open Questions
Decisions needing user input before or during execution.

### 6. Verification Checklist
How to verify the complete implementation after all steps are done.

Do NOT write any code yet. Just create the plan file.`,
          display: true,
        },
        { triggerTurn: true },
      );
    },
  });

  pi.registerCommand("plan-accept", {
    description: "Accept plan and trigger execution with context options",
    handler: async (_args, ctx) => {
      const state = loadState(ctx);
      if (!state) {
        ctx.ui.notify("No active plan found. Use /plan first.", "error");
        return;
      }

      const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());

      if (!fs.existsSync(planPath)) {
        ctx.ui.notify(`Plan file not found: ${planPath}`, "error");
        return;
      }

      const stat = fs.statSync(planPath);
      if (stat.size < 50) {
        ctx.ui.notify("Plan file is too small or empty. Please write a detailed plan first.", "error");
        return;
      }

      const choice = await ctx.ui.select("Accept Plan?", [
        "Accept plan and clear context",
        "Accept plan and compact",
        "Accept plan",
        "Cancel",
      ]);

      if (choice === "Cancel" || choice === undefined) {
        ctx.ui.notify("Accept cancelled. Continue refining the plan.", "info");
        return;
      }

      saveState(ctx, { phase: "idle", approved: true });
      const planContent = fs.readFileSync(planPath, "utf-8");

      if (choice === "Accept plan and clear context") {
        ctx.ui.notify("Plan accepted! Creating new session for fresh execution...", "success");

        const result = await ctx.newSession({
          parentSession: ctx.sessionManager.getSessionFile(),
        });

        if (result.cancelled) {
          ctx.ui.notify("Session creation cancelled.", "info");
          return;
        }

        const newPlanPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
        fs.mkdirSync(path.dirname(newPlanPath), { recursive: true });
        fs.renameSync(planPath, newPlanPath);

        ctx.ui.notify("New session created. Starting plan execution...", "success");
        sendExecutionMessage(planContent);
      } else if (choice === "Accept plan and compact") {
        ctx.ui.notify("Plan accepted! Compacting context for execution...", "success");
        ctx.compact({
          customInstructions: "User has accepted the implementation plan. Summarize the current conversation in a short, concise text focusing on the context needed for plan execution.",
          onComplete: () => {
            ctx.ui.notify("Context compacted. Ready for execution.", "success");
            sendExecutionMessage(planContent);
          },
        });
      } else if (choice === "Accept plan") {
        ctx.ui.notify("Plan accepted! Ready for execution.", "success");
        sendExecutionMessage(planContent);
      }
    },
  });

  pi.registerCommand("plan-show", {
    description: "Display and optionally edit the current plan",
    handler: async (_args, ctx) => {
      const state = loadState(ctx);
      const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
      if (!state || !fs.existsSync(planPath)) {
        ctx.ui.notify("No plan found. Use /plan to create one.", "error");
        return;
      }

      const content = fs.readFileSync(planPath, "utf-8");
      const title = `Plan (${state.approved ? "APPROVED" : "DRAFT"})`;

      const edited = await ctx.ui.editor(title, content);

      if (edited && edited !== content) {
        if (state.approved) {
          ctx.ui.notify("Cannot edit an approved plan. Use /plan to create a new one.", "warning");
        } else {
          fs.writeFileSync(planPath, edited, "utf-8");
          ctx.ui.notify("Plan updated manually.", "success");
        }
      }
    },
  });

  pi.registerCommand("plan-cancel", {
    description: "Cancel plan mode and return to normal mode",
    handler: async (_args, ctx) => {
      const state = loadState(ctx);
      if (!state) {
        ctx.ui.notify("No active plan found.", "error");
        return;
      }

      const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
      const choice = await ctx.ui.select("Cancel Plan?", [
        "Leave plan mode",
        "Leave plan mode and clear plan file",
        "Cancel",
      ]);

      if (choice === "Cancel" || choice === undefined) {
        ctx.ui.notify("Cancel aborted.", "info");
        return;
      }

      saveState(ctx, { phase: "idle", approved: false });

      if (choice === "Leave plan mode and clear plan file") {
        try {
          if (fs.existsSync(planPath)) {
            fs.unlinkSync(planPath);
            ctx.ui.notify("Plan mode cancelled. Plan file deleted.", "success");
          } else {
            ctx.ui.notify("Plan mode cancelled. Plan file already removed.", "success");
          }
        } catch (error) {
          ctx.ui.notify(`Plan cancelled but failed to delete file: ${error}`, "warning");
        }
      } else {
        ctx.ui.notify("Plan mode cancelled. Back to normal mode.", "success");
      }
    },
  });

  pi.on("tool_call", async (event, ctx) => {
    const state = loadState(ctx);
    if (state?.phase !== "plan") return;

    const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());

    if (event.toolName === "write" || event.toolName === "edit") {
      const targetPath = event.input?.path as string | undefined;
      if (targetPath && path.resolve(targetPath) === path.resolve(planPath)) {
        return { block: false };
      }
      return {
        block: true,
        reason: "Plan mode active: Use /plan-accept before implementing code changes",
      };
    }

    if (event.toolName === "bash" && typeof event.input?.command === "string") {
      const cmd = event.input.command;

      // Allow specific safe stderr patterns (read-only)
      const safeStderrPattern = /2>\/dev\/null|2>&1/;
      if (safeStderrPattern.test(cmd)) {
        return { block: false };
      }

      // Block: all > and >> redirections (file creation/modification), sed -i, rm -rf, touch, mv, cp, mkdir
      if (/>|>>|sed -i|rm -rf|touch |mv |cp |mkdir /.test(cmd)) {
        return {
          block: true,
          reason: "Plan mode active: Mutating bash commands are blocked. Use /plan-accept first.",
        };
      }
    }
  });

  pi.on("turn_end", async (_event, ctx) => {
    const state = loadState(ctx);
    if (state?.phase === "plan") {
      ctx.ui.setWidget("plan-mode", [ctx.ui.theme.fg("accent", "󰏯 plan mode")]);
    } else {
      ctx.ui.setWidget("plan-mode", undefined);
    }
  });

  pi.on("before_agent_start", async (event, ctx) => {
    const state = loadState(ctx);
    if (state?.phase !== "plan") return;

    // Inject plan mode reminder as a hidden message (AI sees it, user doesn't)
    return {
      message: {
        customType: "plan-mode-context",
        content: `[Plan mode active - do NOT execute any changes, only read-only exploration and planning]

User instruction: ${event.prompt}`,
        display: false,
      },
    };
  });
}
