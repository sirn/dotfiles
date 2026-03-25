import {
  keyHint,
  type ExtensionAPI,
  type ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import { getExecutionMode } from "../lib/execution-mode.js";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { Container, Text, Box, Spacer } from "@mariozechner/pi-tui";

export default function (pi: ExtensionAPI) {
  const PLAN_DIR = path.join(os.homedir(), ".pi/agent/plans");

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
            theme.fg("muted", `(${keyHint("__KEYBINDING_EXPAND_TOOLS__", "to expand")})`),
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
        content: `The plan has been approved. Execute the implementation plan step by step.

## Rules
- Execute ONE step at a time. Verify success before proceeding. Do NOT combine steps.
- If a step fails, analyze the error. If it fails twice, STOP and wait for user input.
- Keep changes minimal and idiomatic. Only modify in-scope files.
- Verify *results*, not *actions*: validate outcomes (e.g., "test passes", "service works"), not that you performed the step.
- Do not over-verify or verify trivial things (e.g., checking if text exists in a source file you just edited).
- Run the verification checklist after all steps complete.

## Plan content
${planContent}`,
        display: true,
      },
      { triggerTurn: true },
    );
  }

  pi.registerCommand("plan", {
    description: "Create an implementation plan for review",
    handler: async (args, ctx) => {
      const planPath = getPlanPath(
        ctx.cwd,
        ctx.sessionManager.getSessionFile(),
      );
      fs.mkdirSync(path.dirname(planPath), { recursive: true });

      setMode(ctx, "plan");
      updatePlanWidget(ctx);

      // If no args and plan file exists, enter plan mode silently
      if (!args && fs.existsSync(planPath)) {
        ctx.ui.notify("Plan mode active. Existing plan loaded.", "info");
        return;
      }

      pi.sendMessage(
        {
          customType: "plan-mode-prompt",
          content: `Create a detailed implementation/execution plan based on the user instruction.
Write the plan to: ${planPath}

## Pre-Planning Phase (REQUIRED — complete ALL steps in order before writing the plan)

### Step 1: Evaluate Existing Plan
Check whether a plan file already exists at: ${planPath}
- If it exists: read it and evaluate whether it is relevant to the current user request.
  - If relevant: acknowledge it and build on or refine it.
  - If not relevant: discard it (overwrite with a fresh plan).
- If it does not exist: proceed to step 2.

### Step 2: Gather Context
Collect all context necessary to successfully accomplish the request:
- Read the project README and any relevant configuration or source files.
- **Perform a web search**: load the relevant search skill and execute it. Do not guess or assume.
- **Research official documentation** for every library, tool, or API involved. Look up exact call conventions, flags, and return types — do not infer from memory.
- If any requirement is ambiguous or information is missing, **ask the user** before proceeding.

### Step 3: Define Success Criteria
Derive explicit, measurable success criteria from the user request:
- What does "done" look like? (e.g., "tests pass", "command exits 0", "output matches expected format")
- If the criteria are non-obvious or involve tradeoffs, **confirm them with the user** before writing the plan.

### Step 4: Write the Plan
Only after completing steps 1–3, write the plan to: ${planPath}

### Step 5: Verify the Plan
Before finalising, validate the plan's correctness:
- **Re-consult official documentation** to confirm every call convention, option, and API contract used in the plan.
- **Run ad-hoc read-only probes** where helpful (e.g., \`ls\`, \`cat\`, \`--help\`, dry-runs, \`tsc --noEmit\`) to verify assumptions without modifying the system.

## Rules
- CRITICAL: Use ONLY read-only commands for context gathering and verification. Do NOT execute changes.
- Use the \`write\` tool to write the plan file. Do NOT use bash to write files (they will be blocked).
- **Always cite documentation**: include a URL or reference for every external tool, API, or convention referenced in the plan.
- Do NOT write any code yet. Just create the plan file.
- You MUST use only the provided plan file path. Any attempt to write elsewhere will be blocked.

## User instruction
${args || "The requested feature"}

## Plan structure

### Overview
What needs to be built/fixed, why, and how success is measured. What is OUT of scope. Keep the solution minimal.

### Success Criteria
Explicit, measurable criteria that define when the implementation is complete.

### Context
Read-only exploration findings: key files, existing patterns, dependencies, non-obvious details, and relevant URLs.
Example: "\`src/auth.ts\` exports \`createSession(userId)\`; sessions stored in Redis with 24h TTL"

### Implementation Steps
Ordered, atomic, verifiable steps. Each step needs: a clear goal, specific files/changes, and a concrete success criterion (e.g., test command, linter run).
Describe non-trivial changes in sufficient detail to prevent guesswork. Show before/after with ±5 lines of context if helpful.
Example: "Step 1: Add Session type to types.ts — Success: tsc --noEmit passes"

### Verification Checklist
How to verify the complete implementation after all steps are done.

### References
Documentation URLs and sources consulted during planning.

## Policy footer
- Once the plan is written, present a concise summarisation of the plan to the user.`,
          display: true,
          details: {
            userInstruction: args || "The requested feature",
          },
        },
        { triggerTurn: true },
      );
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

  pi.on("turn_end", async (_event, ctx) => {
    updatePlanWidget(ctx);
  });

  pi.on("before_agent_start", async (event, ctx) => {
    if (getMode(ctx) !== "plan") return;

    // Inject plan mode reminder as a hidden message (AI sees it, user doesn't)
    const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());
    return {
      message: {
        customType: "plan-mode-context",
        content: `[Plan mode active - do NOT execute any changes, only read-only exploration and planning; only write a plan to ${planPath}]

User instruction: ${event.prompt}`,
        display: false,
      },
    };
  });
}
