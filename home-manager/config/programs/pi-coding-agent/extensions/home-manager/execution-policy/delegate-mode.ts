import {
  type ExtensionAPI,
  type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import {
  getExecutionMode,
  clearModeCache,
  MODE_DELEGATE,
} from "./lib/execution-mode.js";

const DELEGATE_MODE_PROMPT =
  "[Delegate mode active — You are an orchestrator. You cannot write or edit files directly. All file changes must go through the `subagent` tool. Use `subagent` with a worker agent for code changes.]";

export default function (pi: ExtensionAPI) {
  function getMode(ctx: ExtensionContext): string {
    return getExecutionMode(ctx).mode;
  }

  function hasExecutionModeEntry(ctx: ExtensionContext): boolean {
    for (const entry of ctx.sessionManager.getEntries()) {
      if (entry.type === "custom" && entry.customType === "execution-mode") {
        return true;
      }
    }
    return false;
  }

  function setMode(ctx: ExtensionContext, mode: string) {
    const sessionId = ctx.sessionManager.getSessionFile() ?? "ephemeral";
    clearModeCache(sessionId);
    pi.appendEntry("execution-mode", { mode });
    updateDelegateWidget(ctx);
  }

  function updateDelegateWidget(ctx: ExtensionContext) {
    if (getMode(ctx) === MODE_DELEGATE) {
      ctx.ui.setWidget("delegate-mode", [
        ctx.ui.theme.fg("accent", " delegate"),
      ]);
    } else {
      ctx.ui.setWidget("delegate-mode", undefined);
    }
  }

  // session_start: default into delegate mode if no execution-mode entry exists
  pi.on("session_start", async (_event, ctx) => {
    if (!hasExecutionModeEntry(ctx)) {
      setMode(ctx, MODE_DELEGATE);
    } else {
      updateDelegateWidget(ctx);
    }
  });

  // before_agent_start: inject delegate-mode context reminder
  pi.on("before_agent_start", async (_event, ctx) => {
    if (getMode(ctx) !== MODE_DELEGATE) return;

    return {
      message: {
        customType: "delegate-mode-context",
        content: DELEGATE_MODE_PROMPT,
        display: false,
      },
    };
  });



  // /delegate command: toggle delegate mode (supports 'cancel' subcommand)
  pi.registerCommand("delegate", {
    description:
      "Enter delegate mode — block direct file edits, force subagent delegation; use 'cancel' to exit",
    getArgumentCompletions: (prefix: string) => {
      const token = prefix.trimStart();
      if (token.includes(" ")) return null;
      const subcommands = [
        {
          value: "cancel",
          label: "cancel",
          description: "Exit delegate mode",
        },
      ];
      const filtered = subcommands.filter((s) => s.value.startsWith(token));
      return filtered.length > 0 ? filtered : null;
    },
    handler: async (args, ctx) => {
      const raw = args ?? "";
      const dispatch = raw.trimStart();
      const spaceIdx = dispatch.indexOf(" ");
      const first = spaceIdx === -1 ? dispatch : dispatch.slice(0, spaceIdx);

      if (first === "cancel") {
        if (getMode(ctx) !== MODE_DELEGATE) {
          ctx.ui.notify("Not in delegate mode.", "info");
          return;
        }
        setMode(ctx, "edit");
        ctx.ui.notify("Delegate mode cancelled.", "success");
        return;
      }

      // bare /delegate — enter delegate mode
      if (getMode(ctx) === MODE_DELEGATE) {
        ctx.ui.notify("Already in delegate mode.", "info");
        return;
      }
      setMode(ctx, MODE_DELEGATE);
      ctx.ui.notify(
        "Delegate mode enabled. File edits blocked; use subagent for code changes.",
        "success",
      );
    },
  });

  // Widget update on turn end
  pi.on("turn_end", async (_event, ctx) => {
    updateDelegateWidget(ctx);
  });
}
