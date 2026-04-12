import {
  keyHint,
  type ExtensionAPI,
  type ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import { getExecutionMode } from "./lib/execution-mode.js";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { Container, Text, Box, Spacer } from "@mariozechner/pi-tui";

export default function (pi: ExtensionAPI) {
  const PI_AGENT_DIR = path.join(os.homedir(), ".pi/agent");
  const PLAN_DIR = path.join(PI_AGENT_DIR, "plans");

  const planModePromptTemplate = fs.readFileSync(
    path.join(PI_AGENT_DIR, "PLAN_PROMPT.md"),
    "utf-8",
  );
  const planModeAcceptTemplate = fs.readFileSync(
    path.join(PI_AGENT_DIR, "PLAN_ACCEPT.md"),
    "utf-8",
  );
  const planModeSubsequentTemplate = fs.readFileSync(
    path.join(PI_AGENT_DIR, "PLAN_INJECT.md"),
    "utf-8",
  );

  // --- Mode state management ---

  const modeCache = new Map<string, string>();

  function getMode(ctx: ExtensionContext): string {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    if (modeCache.has(sessionId)) return modeCache.get(sessionId)!;
    const mode = getExecutionMode(ctx).mode;
    modeCache.set(sessionId, mode);
    return mode;
  }

  function setMode(ctx: ExtensionContext, mode: string) {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    modeCache.set(sessionId, mode);
    const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
    pi.appendEntry("execution-mode", {
      mode,
      policyOverride:
        mode === "plan" ? { write: [planPath], edit: [planPath] } : undefined,
    });
  }

  function getPlanContextSent(ctx: ExtensionContext): boolean {
    let sent = false;
    for (const entry of ctx.sessionManager.getEntries()) {
      if (entry.type === "custom" && entry.customType === "plan-context-sent") {
        const data = entry.data as { sent?: boolean };
        sent = data?.sent ?? false;
      }
    }
    return sent;
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

  function updatePlanWidget(ctx: ExtensionContext) {
    if (getMode(ctx) === "plan") {
      ctx.ui.setWidget("plan-mode", [ctx.ui.theme.fg("accent", "󰏯 plan mode")]);
    } else {
      ctx.ui.setWidget("plan-mode", undefined);
    }
  }

  function sendPlanModePrompt(ctx: ExtensionContext, userPrompt: string) {
    const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
    fs.mkdirSync(path.dirname(planPath), { recursive: true });
    pi.appendEntry("plan-context-sent", { sent: true });
    pi.sendMessage(
      {
        customType: "plan-mode-prompt",
        content: planModePromptTemplate
          .replaceAll("{PLAN_PATH}", planPath)
          .replaceAll("{USER_PROMPT}", userPrompt),
        display: true,
        details: { userInstruction: userPrompt },
      },
      { triggerTurn: true },
    );
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
        const text =
          typeof message.content === "string" ? message.content : fallback;
        box.addChild(new Text(text, 0, 0));
      } else {
        const userInstruction = message.details?.userInstruction || fallback;
        box.addChild(new Text(theme.fg(colorKey, theme.bold(header)), 0, 0));
        box.addChild(new Spacer(1));
        box.addChild(new Text(userInstruction, 0, 0));
        box.addChild(new Spacer(1));
        box.addChild(
          new Text(
            theme.fg("muted", `(${keyHint("app.tools.expand", "to expand")})`),
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

  pi.registerMessageRenderer(
    "plan-mode-prompt",
    createMessageRenderer("󰏯 plan mode", "accent", "Plan requested."),
  );

  pi.registerMessageRenderer(
    "plan-mode-execute",
    createMessageRenderer("󰏫 plan approved", "success", "Plan accepted."),
  );

  function sendExecutionMessage(planContent: string) {
    pi.sendMessage(
      {
        customType: "plan-mode-execute",
        content: planModeAcceptTemplate.replaceAll(
          "{PLAN_CONTENT}",
          planContent,
        ),
        display: true,
      },
      { triggerTurn: true },
    );
  }

  pi.registerCommand("plan", {
    description: "Enter plan mode for creating implementation plans",
    handler: async (args, ctx) => {
      const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
      const wasAlreadyInPlanMode = getMode(ctx) === "plan";

      setMode(ctx, "plan");
      updatePlanWidget(ctx);

      // Only reset context flag if we're entering plan mode fresh (not re-entering)
      // If we were already in plan mode, don't reset - continue with existing state
      if (!wasAlreadyInPlanMode) {
        pi.appendEntry("plan-context-sent", { sent: false });
      }

      // If user provided a message, send it as a regular user message
      // The before_agent_start hook will inject planning context on the first message
      if (args) {
        sendPlanModePrompt(ctx, args);
      } else {
        ctx.ui.notify(
          "Plan mode active. Type your request to create a plan.",
          "info",
        );
      }
    },
  });

  pi.registerCommand("plan-accept", {
    description: "Accept plan and trigger execution with context options",
    handler: async (_args, ctx) => {
      if (getMode(ctx) !== "plan") {
        ctx.ui.notify("No active plan found. Use /plan first.", "error");
        return;
      }

      const planPath = getPlanPath(
        ctx.cwd,
        ctx.sessionManager.getSessionFile(),
      );

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

      setMode(ctx, "edit");
      updatePlanWidget(ctx);
      pi.appendEntry("plan-context-sent", { sent: false });
      const planContent = fs.readFileSync(planPath, "utf-8");

      if (choice === "Accept plan and clear context") {
        ctx.ui.notify(
          "Plan accepted! Creating new session for fresh execution...",
          "success",
        );

        const result = await ctx.newSession({
          parentSession: ctx.sessionManager.getSessionFile(),
        });

        if (result.cancelled) {
          ctx.ui.notify("Session creation cancelled.", "info");
          return;
        }

        const newPlanPath = getPlanPath(
          ctx.cwd,
          ctx.sessionManager.getSessionFile(),
        );
        fs.mkdirSync(path.dirname(newPlanPath), { recursive: true });
        fs.renameSync(planPath, newPlanPath);

        ctx.ui.notify(
          "New session created. Starting plan execution...",
          "success",
        );
        sendExecutionMessage(planContent);
      } else if (choice === "Accept plan and compact") {
        ctx.ui.notify(
          "Plan accepted! Compacting context for execution...",
          "success",
        );
        ctx.compact({
          customInstructions:
            "User has accepted the implementation plan. Summarize the current conversation in a short, concise text focusing on the context needed for plan execution.",
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
      const mode = getMode(ctx);
      const planPath = getPlanPath(
        ctx.cwd,
        ctx.sessionManager.getSessionFile(),
      );
      if (mode !== "plan" || !fs.existsSync(planPath)) {
        ctx.ui.notify("No plan found. Use /plan to create one.", "error");
        return;
      }

      const content = fs.readFileSync(planPath, "utf-8");

      const edited = await ctx.ui.editor("Plan", content);

      if (edited && edited !== content) {
        fs.writeFileSync(planPath, edited, "utf-8");
        ctx.ui.notify("Plan updated manually.", "success");
      }
    },
  });

  pi.registerCommand("plan-cancel", {
    description: "Cancel plan mode and return to normal mode",
    handler: async (_args, ctx) => {
      if (getMode(ctx) !== "plan") {
        ctx.ui.notify("No active plan found.", "error");
        return;
      }

      const planPath = getPlanPath(
        ctx.cwd,
        ctx.sessionManager.getSessionFile(),
      );
      const choice = await ctx.ui.select("Cancel Plan?", [
        "Leave plan mode",
        "Leave plan mode and clear plan file",
        "Cancel",
      ]);

      if (choice === "Cancel" || choice === undefined) {
        ctx.ui.notify("Cancel aborted.", "info");
        return;
      }

      setMode(ctx, "edit");
      updatePlanWidget(ctx);
      pi.appendEntry("plan-context-sent", { sent: false });

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
    },
  });

  pi.on("input", async (event, ctx) => {
    if (getMode(ctx) !== "plan") return { action: "continue" };
    if (getPlanContextSent(ctx)) return { action: "continue" };
    // Skip interception for extension-sourced messages
    if (event.source === "extension") return { action: "continue" };

    sendPlanModePrompt(ctx, event.text);
    return { action: "handled" };
  });

  pi.on("turn_end", async (_event, ctx) => {
    updatePlanWidget(ctx);
  });

  pi.on("before_agent_start", async (event, ctx) => {
    if (getMode(ctx) !== "plan") return;

    const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());

    return {
      message: {
        customType: "plan-mode-context",
        content: planModeSubsequentTemplate
          .replaceAll("{PLAN_PATH}", planPath)
          .replaceAll("{USER_PROMPT}", event.prompt),
        display: false,
      },
    };
  });
}
