/**
 * RTK auto-rewrite extension for Pi Coding Agent
 *
 * Overrides the built-in bash tool to run `rtk rewrite` on commands
 * before execution, optimizing token usage.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { createBashTool } from "@mariozechner/pi-coding-agent";

const rtkBin = "__RTK_BIN__";

export default function (pi: ExtensionAPI) {
  const bashTool = createBashTool(process.cwd());

  pi.registerTool({
    ...bashTool,
    async execute(id, params, signal, onUpdate) {
      const cmd = params.command;
      if (cmd && !cmd.includes("<<")) {
        try {
          const result = await pi.exec(rtkBin, ["rewrite", cmd], {
            signal,
            timeout: 5000,
          });
          if (result.code === 0) {
            const rewritten = result.stdout.trim();
            if (rewritten && rewritten !== cmd) {
              params = { ...params, command: rewritten };
            }
          }
        } catch {}
      }
      return bashTool.execute(id, params, signal, onUpdate);
    },
  });
}
