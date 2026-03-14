/* Notify Pi Coding Agent - send desktop notification when agent finishes a turn. */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { execFileSync } from "node:child_process";

const toastifyBin = "__TOASTIFY_BIN__";

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
    try {
      const messages = (event as any).messages || [];
      const raw = extractLastAssistantText(messages);
      const truncated = raw.replace(/\n/g, " ").trim().slice(0, 200);
      const body = truncated || "Pi has finished their turn";
      execFileSync(toastifyBin, ["send", "Pi", body], { stdio: "ignore" });
    } catch {
      try {
        execFileSync(toastifyBin, ["send", "Pi", "Pi has finished their turn"], { stdio: "ignore" });
      } catch {}
    }
  });
}
