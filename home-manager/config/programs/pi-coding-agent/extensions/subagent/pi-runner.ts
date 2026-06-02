/**
 * Pi RPC Runner — spawns a `pi --mode rpc` child process for a subagent task.
 *
 * Extracted from index.ts to enable pluggable agent runners.
 * Communicates via Pi's JSONL RPC protocol over stdin/stdout.
 */

import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionContext } from "@earendil-works/pi-coding-agent";

import type { Message } from "@earendil-works/pi-ai";
import {
  type AgentConfig,
  type SingleResult,
  type SubagentDetails,
  type RpcEvent,
  type OnUpdateCallback,
  createPendingResult,
  getResultErrorMessage,
  writePromptToTempFile,
  cleanupTempPrompt,
  makeEmitUpdate,
} from "./types.js";

// Pi Invocation

function getPiInvocation(args: string[]): { command: string; args: string[] } {
  const currentScript = process.argv[1];
  const isBunVirtualScript = currentScript?.startsWith("/$bunfs/root/");
  if (currentScript && !isBunVirtualScript && fs.existsSync(currentScript)) {
    return { command: process.execPath, args: [currentScript, ...args] };
  }

  const execName = path.basename(process.execPath).toLowerCase();
  const isGenericRuntime = /^(node|bun)(\.exe)?$/.test(execName);
  if (!isGenericRuntime) {
    return { command: process.execPath, args };
  }

  return { command: "pi", args };
}

// Pi RPC Runner

export async function runPiAgent(
  agent: AgentConfig,
  task: string,
  cwd: string,
  parentModes: string[],
  modeOverride: string | undefined,
  signal: AbortSignal | undefined,
  onUpdate: OnUpdateCallback | undefined,
  makeDetails: (results: SingleResult[]) => SubagentDetails,
  ctx: ExtensionContext,
): Promise<SingleResult> {
  const args: string[] = ["--mode", "rpc", "--no-session"];
  if (agent.model) args.push("--model", agent.model);
  if (agent.tools && agent.tools.length > 0)
    args.push("--tools", agent.tools.join(","));

  let tmpPromptDir: string | null = null;
  let tmpPromptPath: string | null = null;

  const currentResult = createPendingResult(agent.name, task);
  currentResult.model = agent.model;
  currentResult.runner = "pi";

  const emitUpdate = makeEmitUpdate(currentResult, onUpdate, makeDetails);

  try {
    if (agent.systemPrompt.trim()) {
      const tmp = await writePromptToTempFile(agent.name, agent.systemPrompt);
      tmpPromptDir = tmp.dir;
      tmpPromptPath = tmp.filePath;
      args.push("--append-system-prompt", tmpPromptPath);
    }

    const promptCommand = {
      id: "subagent-prompt",
      type: "prompt",
      message: `Task: ${task}`,
    };
    let wasAborted = false;
    let spawnErrorMessage: string | undefined;

    const childModes = [...parentModes, "subagent", `subagent:${agent.name}`];
    if (modeOverride) childModes.push(modeOverride);
    const exitCode = await new Promise<number>((resolve) => {
      const invocation = getPiInvocation(args);
      const proc = spawn(invocation.command, invocation.args, {
        cwd,
        shell: false,
        stdio: ["pipe", "pipe", "pipe"],
        env: {
          ...process.env,
          PI_EXECUTION_MODE: childModes.join(","),
        },
      });
      let buffer = "";
      let killTimeout: ReturnType<typeof setTimeout> | undefined;
      let settled = false;
      let agentEndTimer: ReturnType<typeof setTimeout> | undefined;

      const sendRpc = (message: Record<string, unknown>) => {
        if (!proc.stdin || proc.stdin.destroyed || proc.stdin.writableEnded)
          return;
        try {
          proc.stdin.write(`${JSON.stringify(message)}\n`);
        } catch {
          /* ignore closed RPC stdin */
        }
      };

      const killProc = () => {
        wasAborted = true;
        currentResult.stopReason = "aborted";
        currentResult.errorMessage = "Subagent was aborted";
        sendRpc({ type: "abort" });
        proc.kill("SIGTERM");
        killTimeout = setTimeout(() => {
          if (!proc.killed) proc.kill("SIGKILL");
        }, 5000);
      };

      const cleanup = () => {
        if (killTimeout) clearTimeout(killTimeout);
        if (agentEndTimer !== undefined) clearTimeout(agentEndTimer);
        if (signal) signal.removeEventListener("abort", killProc);
      };

      const finish = (code: number) => {
        if (settled) return;
        settled = true;
        cleanup();
        resolve(code);
      };

      const finishSuccess = () => {
        if (proc.stdin && !proc.stdin.destroyed && !proc.stdin.writableEnded) {
          proc.stdin.end();
        }
        finish(0);
        proc.kill("SIGTERM");
      };

      const finishFailure = () => {
        if (proc.stdin && !proc.stdin.destroyed && !proc.stdin.writableEnded) {
          proc.stdin.end();
        }
        finish(1);
        proc.kill("SIGTERM");
      };

      const clearAgentEndTimer = () => {
        if (agentEndTimer !== undefined) {
          clearTimeout(agentEndTimer);
          agentEndTimer = undefined;
        }
      };

      // Pi may auto-retry or compact-and-retry after agent_end, so defer
      // finalization with a grace period that a late retry can cancel.
      const scheduleGraceFinish = () => {
        clearAgentEndTimer();
        agentEndTimer = setTimeout(() => {
          agentEndTimer = undefined;
          currentResult.autoRetrying = false;
          currentResult.compacting = false;
          finishSuccess();
        }, 2000);
      };

      let lineQueue = Promise.resolve();

      const handleExtensionUIRequest = async (
        event: Extract<RpcEvent, { type: "extension_ui_request" }>,
      ) => {
        const { id, method } = event;
        switch (method) {
          case "select": {
            const value = await ctx.ui.select(
              event.title ?? event.message ?? "Select",
              event.options ?? [],
            );
            sendRpc(
              value === undefined
                ? { type: "extension_ui_response", id, cancelled: true }
                : { type: "extension_ui_response", id, value },
            );
            break;
          }
          case "confirm": {
            const value = await ctx.ui.select(
              event.message
                ? `${event.title ?? "Confirm"}: ${event.message}`
                : (event.title ?? "Confirm"),
              ["Yes", "No"],
            );
            sendRpc(
              value === undefined
                ? { type: "extension_ui_response", id, cancelled: true }
                : {
                    type: "extension_ui_response",
                    id,
                    confirmed: value === "Yes",
                  },
            );
            break;
          }
          case "input":
          case "editor":
            sendRpc({ type: "extension_ui_response", id, cancelled: true });
            break;
          case "notify":
            if (event.message) {
              const notifyType =
                event.notifyType === "warning" || event.notifyType === "error"
                  ? event.notifyType
                  : "info";
              ctx.ui.notify(event.message, notifyType);
            }
            break;
          default:
            break;
        }
      };

      const processLine = async (line: string) => {
        if (!line.trim()) return;
        let event: RpcEvent;
        try {
          event = JSON.parse(line);
        } catch {
          return;
        }

        // Work-producing events cancel the deferred finalization timer.
        const isWorkEvent =
          event.type === "message_end" ||
          event.type === "message_start" ||
          event.type === "message_delta" ||
          event.type === "tool_execution_start" ||
          event.type === "tool_execution_end" ||
          event.type === "turn_start" ||
          event.type === "turn_end";
        if (isWorkEvent) clearAgentEndTimer();

        if (event.type === "message_end" && event.message) {
          const msg = event.message as Message;
          if (msg.role === "toolResult" && !msg.isError) {
            msg.content = [];
          }
          currentResult.messages.push(msg);

          if (msg.role === "assistant") {
            currentResult.usage.turns++;
            const usage = msg.usage;
            if (usage) {
              currentResult.usage.input += usage.input || 0;
              currentResult.usage.output += usage.output || 0;
              currentResult.usage.cacheRead += usage.cacheRead || 0;
              currentResult.usage.cacheWrite += usage.cacheWrite || 0;
              currentResult.usage.cost += usage.cost?.total || 0;
              currentResult.usage.contextTokens = usage.totalTokens || 0;
            }
            if (!currentResult.model && msg.model)
              currentResult.model = msg.model;
            if (msg.stopReason) currentResult.stopReason = msg.stopReason;
            if (msg.errorMessage) currentResult.errorMessage = msg.errorMessage;
          }
          emitUpdate();
        }

        if (event.type === "tool_execution_end" && event.isError) {
          currentResult.errorMessage ||= getResultErrorMessage({
            ...currentResult,
            messages: [],
            errorMessage:
              typeof event.result?.content === "string"
                ? event.result.content
                : undefined,
          });
        }

        if (event.type === "extension_ui_request") {
          await handleExtensionUIRequest(event);
        }

        if (event.type === "agent_end") {
          if (settled) return;
          scheduleGraceFinish();
          return;
        }

        // Built-in auto-retry
        if (event.type === "auto_retry_start") {
          if (settled) return;
          clearAgentEndTimer();
          currentResult.errorMessage = event.errorMessage;
          currentResult.autoRetrying = true;
          emitUpdate();
        }

        if (event.type === "auto_retry_end") {
          if (settled) return;
          if (event.success) {
            clearAgentEndTimer();
            currentResult.errorMessage = undefined;
            currentResult.stopReason = undefined;
            currentResult.autoRetrying = false;
          } else {
            currentResult.stopReason = "error";
            currentResult.errorMessage =
              event.finalError || currentResult.errorMessage;
            clearAgentEndTimer();
            finishFailure();
          }
          emitUpdate();
        }

        // Compaction (context overflow recovery)
        if (event.type === "compaction_start") {
          if (settled) return;
          clearAgentEndTimer();
          currentResult.compacting = true;
          emitUpdate();
        }

        if (event.type === "compaction_end") {
          if (settled) return;
          currentResult.compacting = false;
          if (!event.willRetry) {
            const isOverflowRecovery = event.reason !== "threshold";
            if (isOverflowRecovery && (event.aborted || event.errorMessage)) {
              currentResult.stopReason = "error";
              currentResult.errorMessage =
                event.errorMessage || currentResult.errorMessage;
              clearAgentEndTimer();
              finishFailure();
            } else {
              // Threshold or successful compaction — restart the grace timer.
              scheduleGraceFinish();
            }
          }
          emitUpdate();
        }

        // Prompt preflight failure (no agent_end will follow)
        if (
          event.type === "response" &&
          event.id === "subagent-prompt" &&
          !event.success
        ) {
          if (settled) return;
          currentResult.errorMessage = event.error || "Prompt preflight failed";
          currentResult.stopReason = "error";
          clearAgentEndTimer();
          finishFailure();
        }
      };

      const enqueueLine = (line: string) => {
        lineQueue = lineQueue.then(() => processLine(line));
        return lineQueue;
      };

      proc.stdout.on("data", (data) => {
        buffer += data.toString();
        const lines = buffer.split("\n");
        buffer = lines.pop() || "";
        for (const line of lines) enqueueLine(line);
      });

      proc.stderr.on("data", (data) => {
        currentResult.stderr += data.toString();
      });

      proc.on("close", (code, childSignal) => {
        if (buffer.trim()) enqueueLine(buffer);
        lineQueue.then(
          () => {
            if (typeof code === "number") finish(code);
            else if (wasAborted || childSignal) finish(130);
            else finish(1);
          },
          (error) => {
            currentResult.errorMessage =
              error instanceof Error ? error.message : String(error);
            currentResult.stderr += `${currentResult.errorMessage}\n`;
            finish(1);
          },
        );
      });

      proc.on("error", (error) => {
        spawnErrorMessage = `Failed to start pi for agent "${agent.name}": ${error.message}`;
        currentResult.errorMessage = spawnErrorMessage;
        currentResult.stderr += `${spawnErrorMessage}\n`;
        finish(1);
      });

      sendRpc(promptCommand);

      if (signal) {
        if (signal.aborted) killProc();
        else signal.addEventListener("abort", killProc, { once: true });
      }
    });

    currentResult.exitCode = exitCode;
    if (wasAborted) {
      currentResult.stopReason = "aborted";
      currentResult.errorMessage ||= "Subagent was aborted";
      return currentResult;
    }
    if (spawnErrorMessage) currentResult.errorMessage = spawnErrorMessage;
    if (exitCode !== 0 && !currentResult.errorMessage) {
      currentResult.errorMessage = `Subagent process exited with code ${exitCode}`;
    }
    return currentResult;
  } finally {
    cleanupTempPrompt(tmpPromptDir, tmpPromptPath);
  }
}
