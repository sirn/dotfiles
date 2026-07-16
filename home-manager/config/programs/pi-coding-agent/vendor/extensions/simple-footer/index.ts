import type { AssistantMessage } from "@earendil-works/pi-ai";
import * as path from "node:path";
import {
  type ExtensionAPI,
  type SessionEntry,
  getLatestCompactionEntry,
} from "@earendil-works/pi-coding-agent";
import type { TUI } from "@earendil-works/pi-tui";
function formatCwd(cwd: string): string {
  const home = process.env.HOME;
  if (home && (cwd === home || cwd.startsWith(home + path.sep)))
    return `~${cwd.slice(home.length)}`;
  return cwd;
}

function formatTokens(n: number): string {
  if (n < 1000) return String(n);
  if (n < 10000) return `${(n / 1000).toFixed(1)}k`;
  if (n < 1000000) return `${Math.round(n / 1000)}k`;
  return `${(n / 1000000).toFixed(1)}M`;
}

// Strip ANSI escape sequences for visible width.
function visibleWidth(s: string): number {
  return s.replace(/\x1b\[[0-9;]*m/g, "").length;
}

// Truncate to visible width, optionally with ellipsis.
function truncateToWidth(
  text: string,
  maxWidth: number,
  ellipsis = "",
): string {
  const eLen = visibleWidth(ellipsis);
  const tLen = visibleWidth(text);
  if (tLen <= maxWidth) return text;
  const target = maxWidth - eLen;
  let out = "";
  let outLen = 0;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (ch === "\x1b") {
      const end = text.indexOf("m", i);
      if (end !== -1) {
        out += text.slice(i, end + 1);
        i = end;
        continue;
      }
    }
    if (outLen >= target) break;
    out += ch;
    outLen++;
  }
  return out + ellipsis;
}

function isRecentlyCompacted(branch: SessionEntry[]): boolean {
  const latest = getLatestCompactionEntry(branch);
  if (!latest) return false;
  const leaf = branch[branch.length - 1];
  if (leaf && leaf.id === latest.id) return true;
  const idx = branch.lastIndexOf(latest);
  return idx >= 0 && branch.length - idx <= 3;
}

export default function (pi: ExtensionAPI) {
  let activeTui: TUI | undefined;

  pi.on("session_start", (_event, ctx) => {
    if (!ctx.hasUI) return;

    let usage = {
      input: 0,
      output: 0,
      cacheRead: 0,
      cacheWrite: 0,
      cost: 0,
      compacted: false,
    };

    const recomputeUsage = () => {
      let input = 0,
        output = 0,
        cacheRead = 0,
        cacheWrite = 0,
        cost = 0;
      const branch = ctx.sessionManager.getBranch();
      for (const entry of branch) {
        if (entry.type === "message" && entry.message?.role === "assistant") {
          const msg = entry.message as AssistantMessage;
          input += msg.usage.input;
          output += msg.usage.output;
          cacheRead += msg.usage.cacheRead;
          cacheWrite += msg.usage.cacheWrite;
          cost += msg.usage.cost.total;
        }
      }
      usage = {
        input,
        output,
        cacheRead,
        cacheWrite,
        cost,
        compacted: isRecentlyCompacted(branch),
      };
    };

    ctx.ui.setFooter((tui, theme, footerData) => {
      activeTui = tui;

      const unsubBranch = footerData.onBranchChange(() => {
        recomputeUsage();
        tui.requestRender();
      });
      recomputeUsage();

      // Cache the last rendered footer keyed by width + state signature
      // so repeated renders within the same turn skip recomputation.
      let renderCache:
        | { sig: string; width: number; lines: string[] }
        | undefined;
      return {
        dispose() {
          unsubBranch();
          activeTui = undefined;
        },
        invalidate() {
          renderCache = undefined;
        },
        render(width: number): string[] {
          const { input, output, cacheRead, cacheWrite, cost, compacted } =
            usage;
          const ctxUsage = ctx.getContextUsage();

          // Cross-extension statuses.
          const statuses = footerData.getExtensionStatuses();
          const execMode = statuses.get("execution-mode") ?? null;
          const subCost = statuses.get("subagent-cost") ?? null;
          const goalStatus = statuses.get("goal-status") ?? null;

          // Reuse cached footer when width and all dynamic inputs are unchanged.
          const sig = [
            ctx.cwd,
            input,
            output,
            cacheRead,
            cacheWrite,
            cost.toFixed(2),
            ctxUsage?.tokens,
            ctxUsage?.contextWindow,
            ctx.model?.id,
            pi.getThinkingLevel(),
            compacted,
            execMode,
            subCost,
            goalStatus,
          ].join("|");
          if (
            renderCache &&
            renderCache.width === width &&
            renderCache.sig === sig
          ) {
            return renderCache.lines;
          }

          // Line 1: cwd | statuses.
          let l1Left = theme.fg("dim", formatCwd(ctx.cwd));

          const l1RightParts: string[] = [];
          if (execMode) l1RightParts.push(theme.fg("accent", execMode));
          if (goalStatus) l1RightParts.push(theme.fg("muted", goalStatus));
          const l1Right = l1RightParts.join(" ");

          // Line 2: tokens cost (subcost) ctx (compact) | model.
          const parts: string[] = [];
          if (input) parts.push(theme.fg("dim", `↑${formatTokens(input)}`));
          if (output) parts.push(theme.fg("dim", `↓${formatTokens(output)}`));
          if (cacheRead)
            parts.push(theme.fg("dim", `R${formatTokens(cacheRead)}`));
          if (cacheWrite)
            parts.push(theme.fg("dim", `W${formatTokens(cacheWrite)}`));
          if (cost) parts.push(theme.fg("dim", `$${cost.toFixed(2)}`));
          if (subCost) parts.push(theme.fg("dim", `(${subCost})`));
          if (ctxUsage?.tokens != null && ctxUsage.contextWindow > 0) {
            const ctxStr = `ctx:${formatTokens(ctxUsage.tokens)}/${formatTokens(ctxUsage.contextWindow)}`;
            parts.push(theme.fg("dim", ctxStr));
          }
          if (compacted) {
            parts.push(theme.fg("warning", "(compact)"));
          }
          const l2Left = parts.join(" ");

          let l2Right = "";
          if (ctx.model) {
            l2Right = theme.fg("dim", ctx.model.id);
            const thinking = pi.getThinkingLevel();
            if (thinking && thinking !== "off") {
              l2Right += " " + theme.fg("dim", `• ${thinking}`);
            }
          }

          // Pad and truncate.
          const l1w = visibleWidth(l1Left);
          const r1w = visibleWidth(l1Right);
          const l2w = visibleWidth(l2Left);
          const r2w = visibleWidth(l2Right);
          const pad1 = " ".repeat(Math.max(1, width - l1w - r1w));
          const pad2 = " ".repeat(Math.max(1, width - l2w - r2w));

          const lines = [
            truncateToWidth(l1Left + pad1 + l1Right, width),
            truncateToWidth(l2Left + pad2 + l2Right, width),
          ];
          renderCache = { sig, width, lines };
          return lines;
        },
      };
    });

    pi.on("turn_end", () => {
      recomputeUsage();
      activeTui?.requestRender();
    });

    pi.on("agent_end", () => {
      recomputeUsage();
      activeTui?.requestRender();
    });
  });

  pi.on("session_shutdown", (_event, ctx) => {
    if (ctx.hasUI) {
      ctx.ui.setFooter(undefined);
    }
  });
}
