import {
  type ExtensionAPI,
  type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import {
  MODE_EDIT,
  getMode,
  setMode,
  EXECUTION_MODE_ENTRY,
  registerMode,
  updateModeWidgets,
} from "./lib/execution-mode.js";

export const MODE_DELEGATE = "delegate";
const DELEGATE_MODE_PROMPT =
  "[Delegate mode active — You are an orchestrator. You may not run a command, write, or edit a file. Do not make a decision on your own. Delegate your tasks to subagent using `subagent` tool.]";

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

  // Registration

  pi.on("session_start", async (_event, ctx) => {
    if (!hasExecutionModeEntry(ctx)) {
      setMode(pi, ctx, MODE_DELEGATE);
      updateModeWidgets(ctx);
    } else {
      updateModeWidgets(ctx);
    }
  });

  pi.registerCommand("delegate", {
    description:
      "Enter delegate mode — force subagent delegation for file changes; use 'cancel' to exit",
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
        updateModeWidgets(ctx);
        ctx.ui.notify("Delegate mode cancelled.", "success");
        return;
      }

      // bare /delegate — enter delegate mode
      if (getMode(ctx) === MODE_DELEGATE) {
        ctx.ui.notify("Already in delegate mode.", "info");
        return;
      }

      setMode(pi, ctx, MODE_DELEGATE);
      updateModeWidgets(ctx);
      ctx.ui.notify(
        "Delegate mode enabled. File changes must go through subagent.",
        "success",
      );
    },
  });

  pi.on("turn_end", async (_event, ctx) => {
    updateModeWidgets(ctx);
  });

  registerMode({
    mode: MODE_DELEGATE,
    getPrompt: () => DELEGATE_MODE_PROMPT,
    widget: {
      widgetId: "delegate-mode",
      label: "\uF454 delegate",
    },
  });
}
