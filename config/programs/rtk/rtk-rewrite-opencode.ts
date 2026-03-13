/**
 * RTK auto-rewrite plugin for OpenCode
 *
 * Rewrites bash commands via `rtk rewrite` before execution,
 * optimizing token usage.
 */

import type { Plugin } from "@opencode-ai/plugin"

const rtkBin = "__RTK_BIN__"

export const RtkRewrite: Plugin = async ({ $ }) => {
  return {
    "tool.execute.before": async (input, output) => {
      if (input.tool !== "bash") return
      const cmd = output.args.command as string
      if (!cmd || cmd.includes("<<")) return
      try {
        const rewritten = await $`${rtkBin} rewrite ${cmd}`.text()
        const trimmed = rewritten.trim()
        if (trimmed && trimmed !== cmd) {
          output.args.command = trimmed
        }
      } catch {}
    },
  }
}
