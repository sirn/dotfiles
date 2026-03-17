import {
  keyHint,
  type ExtensionAPI,
  type ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { Container, Text, Box, Spacer } from "@mariozechner/pi-tui";
import {
  evaluateCommand,
  evaluateRedirects,
  evaluateHeredocs,
  extractCommands,
  tokenize,
  buildWrapperRuleMap,
  normalizeUnifiedPolicyConfig,
  getCommandSummary,
  type PolicyCommands,
  type RedirectPolicy,
  type HeredocPolicy,
  type WrapperRuleConfig,
} from "../lib/shell-policy.js";

async function confirmCommand(
  cmd: string,
  ctx: ExtensionContext,
  promptPrefix: string,
): Promise<{ block: boolean; reason?: string }> {
  if (!ctx.hasUI) {
    return {
      block: true,
      reason: `${promptPrefix} blocked (no UI): "${getCommandSummary(cmd)}"`,
    };
  }
  const choice = await ctx.ui.select(
    `${promptPrefix}: ${getCommandSummary(cmd)}`,
    ["Yes, proceed", "No, cancel"],
  );
  if (choice !== "Yes, proceed") {
    ctx.ui.notify("Command cancelled by user", "info");
    return { block: true, reason: "Blocked by user" };
  }
  return { block: false };
}

// --- Policy types ---

interface PlanPolicy {
  tools: Record<string, boolean>;
  commands: PolicyCommands;
  wrappers?: WrapperRuleConfig[];
  redirects?: RedirectPolicy;
  heredocs?: HeredocPolicy;
}

function loadPlanPolicy(): PlanPolicy | null {
  try {
    const extDir = path.dirname(fileURLToPath(import.meta.url));
    const raw = JSON.parse(
      fs.readFileSync(path.join(extDir, "../../../policy.json"), "utf-8"),
    );
    const unified = normalizeUnifiedPolicyConfig(raw);
    const planMode = unified.modes?.plan;
    if (!planMode) return null;
    return {
      tools: planMode.tools ?? {},
      commands: planMode.commands,
      wrappers: planMode.wrappers,
      redirects: planMode.redirects,
      heredocs: planMode.heredocs,
    };
  } catch {
    return null;
  }
}

// Loaded once at extension init
const planPolicy = loadPlanPolicy();

// --- Plan state management ---

interface PlanState {
  phase: "idle" | "plan";
  approved: boolean;
}

export default function (pi: ExtensionAPI) {
  const PLAN_DIR = path.join(os.homedir(), ".pi/agent/plans");

  const stateMap = new Map<string, PlanState | null>();

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

  function loadState(ctx: ExtensionContext): PlanState | null {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    if (stateMap.has(sessionId)) return stateMap.get(sessionId)!;

    let state: PlanState | null = null;
    for (const entry of ctx.sessionManager.getEntries()) {
      if (entry.type === "custom" && entry.customType === "plan-mode") {
        state = entry.data as PlanState;
      }
    }

    stateMap.set(sessionId, state);
    return state;
  }

  function saveState(ctx: ExtensionContext, state: PlanState) {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    stateMap.set(sessionId, state);
    pi.appendEntry("plan-mode", state);
  }

  function updatePlanWidget(ctx: ExtensionContext) {
    const state = loadState(ctx);
    if (state?.phase === "plan") {
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
            theme.fg("muted", `(${keyHint("expandTools", "to expand")})`),
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
- Skip trivial text verifications (e.g., checking if a string was added) unless doing mass/complex changes.
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

      saveState(ctx, { phase: "plan", approved: false });
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
      const state = loadState(ctx);
      if (!state) {
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

      saveState(ctx, { phase: "idle", approved: true });
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
      const state = loadState(ctx);
      const planPath = getPlanPath(
        ctx.cwd,
        ctx.sessionManager.getSessionFile(),
      );
      if (!state || !fs.existsSync(planPath)) {
        ctx.ui.notify("No plan found. Use /plan to create one.", "error");
        return;
      }

      const content = fs.readFileSync(planPath, "utf-8");
      const title = `Plan (${state.approved ? "APPROVED" : "DRAFT"})`;

      const edited = await ctx.ui.editor(title, content);

      if (edited && edited !== content) {
        if (state.approved) {
          ctx.ui.notify(
            "Cannot edit an approved plan. Use /plan to create a new one.",
            "warning",
          );
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

      saveState(ctx, { phase: "idle", approved: false });
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

  pi.on("tool_call", async (event, ctx) => {
    const state = loadState(ctx);
    if (state?.phase !== "plan") return;

    const planPath = getPlanPath(ctx.cwd, ctx.sessionManager.getSessionFile());

    // Tool-level blocking from [mode.plan.tools]
    if (event.toolName === "write" || event.toolName === "edit") {
      const targetPath = event.input?.path as string | undefined;
      if (targetPath && path.resolve(targetPath) === path.resolve(planPath)) {
        return { block: false };
      }
      const toolAllowed = planPolicy?.tools[event.toolName] ?? false;
      if (!toolAllowed) {
        return {
          block: true,
          reason:
            "Plan mode active: Use /plan-accept before implementing code changes",
        };
      }
    }

    // Bash command blocking from [mode.plan.commands]
    if (event.toolName === "bash" && typeof event.input?.command === "string") {
      if (!planPolicy) {
        return {
          block: true,
          reason: "Plan mode: Shell commands blocked (plan policy unavailable)",
        };
      }
      const cmd = event.input.command;
      const wrapperRules = buildWrapperRuleMap(planPolicy.wrappers);

      // First evaluate against the JSON policy
      const result = evaluateCommand(cmd, planPolicy.commands, wrapperRules);

      // If already denied by JSON policy, return that result
      if (result.action === "deny") {
        return {
          block: true,
          reason: `Plan mode: Command blocked by policy: "${getCommandSummary(cmd)}"`,
        };
      }

      // Check redirect and heredoc policies on extracted commands
      const extractedCmds = extractCommands(
        tokenize(cmd),
        "direct",
        wrapperRules,
      );

      if (planPolicy.redirects) {
        const redirectResult = evaluateRedirects(
          extractedCmds,
          planPolicy.redirects,
        );
        if (redirectResult.action === "deny") {
          return {
            block: true,
            reason: `Plan mode: Command with file output redirect blocked: "${getCommandSummary(cmd)}"`,
          };
        }
        if (redirectResult.action === "ask") {
          const confirmation = await confirmCommand(
            cmd,
            ctx,
            "Plan mode confirm",
          );
          if (confirmation.block) return confirmation;
        }
      }

      if (planPolicy.heredocs) {
        const heredocResult = evaluateHeredocs(
          extractedCmds,
          planPolicy.heredocs,
        );
        if (heredocResult.action === "deny") {
          return {
            block: true,
            reason: `Plan mode: Heredoc command blocked: "${getCommandSummary(cmd)}"`,
          };
        }
        if (heredocResult.action === "ask") {
          const confirmation = await confirmCommand(
            cmd,
            ctx,
            "Plan mode confirm",
          );
          if (confirmation.block) return confirmation;
        }
      }

      // Handle ask/allow/default from policy evaluation
      switch (result.action) {
        case "ask": {
          const confirmation = await confirmCommand(
            cmd,
            ctx,
            "Plan mode confirm",
          );
          if (confirmation.block) {
            return confirmation;
          }
          return { block: false };
        }
        case "allow":
          return { block: false };
        case "default":
          // No match → return undefined → safety-gate handles defaults
          return undefined;
      }
    }
  });

  pi.on("turn_end", async (_event, ctx) => {
    updatePlanWidget(ctx);
  });

  pi.on("before_agent_start", async (event, ctx) => {
    const state = loadState(ctx);
    if (state?.phase !== "plan") return;

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
