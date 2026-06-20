/* Notify Pi Coding Agent - emit OSC 9 when agent finishes a turn.
 *
 * OSC 9 (`ESC ] 9 ; <message> BEL`) is interpreted by terminals like
 * Ghostty, Kitty, and iTerm2 as a native desktop notification request.
 * See https://ghostty.org/docs/vt/osc/9.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

function extractLastAssistantText(messages: any[]): string {
  const lastAssistant = messages.filter((m) => m.role === "assistant").pop();
  if (!lastAssistant?.content) return "";
  if (typeof lastAssistant.content === "string") return lastAssistant.content;
  if (Array.isArray(lastAssistant.content)) {
    return lastAssistant.content
      .filter((block: any) => block.type === "text")
      .map((block: any) => block.text)
      .join("");
  }
  return "";
}

export default function (pi: ExtensionAPI) {
  pi.on("agent_end", (event) => {
    const messages = event.messages || [];
    const raw = extractLastAssistantText(messages);
    const truncated = raw.replace(/\n/g, " ").trim().slice(0, 200);
    const body = truncated || "Pi has finished their turn";
    const osc9 = `\x1b]9;${body}\x07`;
    // When inside tmux, raw OSC sequences are dropped by tmux unless wrapped
    // in the DCS passthrough envelope (\ePtmux;...\e\\) with ESC doubled.
    // allow-passthrough must be enabled in tmux config.
    const output = process.env.TMUX
      ? `\x1bPtmux;${osc9.replace(/\x1b/g, "\x1b\x1b")}\x1b\\`
      : osc9;
    process.stdout.write(output);
  });
}
