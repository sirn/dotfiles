import {
  type ExtensionAPI,
  type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import {
  MODE_DELEGATE,
  MODE_EDIT,
  getMode,
  setMode,
  EXECUTION_MODE_ENTRY,
} from "./lib/execution-mode.js";

const DELEGATE_MODE_PROMPT =
  "[Delegate mode active — You are an orchestrator. You cannot write or edit files directly. Delegate your tasks to subagent using `subagent` tool.]";

export default function (pi: ExtensionAPI) {
  function hasExecutionModeEntry(ctx: ExtensionContext): boolean {
    for (const entry of ctx.sessionManager.getEntries()) {
      if (
        entry.type === "custom" &&
        entry.customType === EXECUTION_MODE_ENTRY
      ) {
        return true;
      }
    }
    return false;
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

  // Registration

  pi.on("session_start", async (_event, ctx) => {
    if (!hasExecutionModeEntry(ctx)) {
      setMode(pi, ctx, MODE_DELEGATE);
      updateDelegateWidget(ctx);
    } else {
      updateDelegateWidget(ctx);
    }
  });

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
        setMode(pi, ctx, MODE_EDIT);
        updateDelegateWidget(ctx);
        ctx.ui.notify("Delegate mode cancelled.", "success");
        return;
      }

      // bare /delegate — enter delegate mode
      if (getMode(ctx) === MODE_DELEGATE) {
        ctx.ui.notify("Already in delegate mode.", "info");
        return;
      }

      setMode(pi, ctx, MODE_DELEGATE);
      updateDelegateWidget(ctx);
      ctx.ui.notify(
        "Delegate mode enabled. File edits blocked; use subagent for code changes.",
        "success",
      );
    },
  });

  pi.on("turn_end", async (_event, ctx) => {
    updateDelegateWidget(ctx);
  });
}
