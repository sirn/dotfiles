/**
 * Claude Code Runner — spawns `claude --print` for a subagent task.
 *
 * Uses Claude Code's `--output-format stream-json` mode to capture
 * structured events (assistant messages, tool calls, final result)
 * and translate them into the shared SingleResult format.
 *
 * Differences from Pi RPC runner:
 * - No stdin abort protocol; uses process signals (SIGTERM → SIGKILL)
 * - No extension UI proxying; CC --print is fully non-interactive
 * - No auto-retry / compaction events; CC handles those internally
 * - Definitive `result` event replaces Pi's `agent_end` + grace timer
 */

import { spawn } from "node:child_process";
import type { Message } from "@earendil-works/pi-ai";
import type { ExtensionContext } from "@earendil-works/pi-coding-agent";
import {
  type AgentConfig,
  type SingleResult,
  type SubagentDetails,
  type OnUpdateCallback,
  type CCEvent,
  type CCAssistantEvent,
  type CCResultEvent,
  createPendingResult,
  getFinalOutput,
  writePromptToTempFile,
  cleanupTempPrompt,
  makeEmitUpdate,
} from "./types.js";

// Claude Code CLI invocation

function getClaudeCodeInvocation(args: string[]): {
  command: string;
  args: string[];
} {
  return { command: "claude", args };
}

// Event parsing helpers

/**
 * Map Claude Code stop reasons to Pi stop reasons.
 */
function mapCCStopReason(
  reason: string | undefined | null,
): Message["stopReason"] {
  const map: Record<string, Message["stopReason"]> = {
    end_turn: "stop",
    tool_use: "toolUse",
    max_tokens: "length",
    stop_sequence: "stop",
  };
  return map[reason ?? ""] ?? "stop";
}

/**
 * Parse a single NDJSON line from Claude Code's stdout.
 * Returns null for empty/invalid lines.
 */
function parseCCLine(line: string): CCEvent | null {
  if (!line.trim()) return null;
  try {
    return JSON.parse(line) as CCEvent;
  } catch {
    return null;
  }
}

/**
 * Extract the text content from an assistant message.
 * Skips thinking blocks; includes text and tool_use content blocks.
 */
function extractAssistantContent(event: CCAssistantEvent): {
  textParts: string[];
  toolCalls: Array<{
    name: string;
    id: string;
    input: Record<string, unknown>;
  }>;
} {
  const textParts: string[] = [];
  const toolCalls: Array<{
    name: string;
    id: string;
    input: Record<string, unknown>;
  }> = [];

  if (!event.message?.content) return { textParts, toolCalls };

  for (const block of event.message.content) {
    if (block.type === "text" && "text" in block) {
      textParts.push((block as { type: "text"; text: string }).text);
    } else if (block.type === "tool_use" && "name" in block && "id" in block) {
      toolCalls.push({
        name: (block as { name: string }).name,
        id: (block as { id: string }).id,
        input: (block as { input: Record<string, unknown> }).input ?? {},
      });
    }
    // Skip thinking blocks — they're internal to Claude Code
  }

  return { textParts, toolCalls };
}

/**
 * Build a Pi-compatible AssistantMessage from a Claude Code assistant event.
 * Combines text and tool_use content blocks.
 */
function buildAssistantMessage(event: CCAssistantEvent): Message {
  const { textParts, toolCalls } = extractAssistantContent(event);

  const content: Message["content"] = [];

  for (const text of textParts) {
    content.push({ type: "text", text });
  }

  for (const tc of toolCalls) {
    content.push({
      type: "toolCall",
      id: tc.id,
      name: tc.name,
      arguments: tc.input,
    });
  }

  // Claude Code messages lack api/provider — fill with placeholders
  const api = "anthropic-messages";
  const provider = "anthropic";
  const model = event.message.model ?? "unknown";
  const usage = event.message.usage
    ? {
        input: event.message.usage.input_tokens ?? 0,
        output: event.message.usage.output_tokens ?? 0,
        cacheRead: event.message.usage.cache_read_input_tokens ?? 0,
        cacheWrite: event.message.usage.cache_creation_input_tokens ?? 0,
        totalTokens:
          (event.message.usage.input_tokens ?? 0) +
          (event.message.usage.output_tokens ?? 0) +
          (event.message.usage.cache_read_input_tokens ?? 0) +
          (event.message.usage.cache_creation_input_tokens ?? 0),
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
      }
    : {
        input: 0,
        output: 0,
        cacheRead: 0,
        cacheWrite: 0,
        totalTokens: 0,
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
      };

  const stopReason: Message["stopReason"] = mapCCStopReason(
    event.message.stop_reason,
  );

  return {
    role: "assistant",
    content,
    api,
    provider,
    model,
    usage,
    stopReason,
    timestamp: Date.now(),
  };
}

/**
 * Build Pi-compatible ToolResultMessage(s) from a Claude Code user event.
 * Each tool_result block becomes a separate ToolResultMessage.
 */
function buildToolResultMessages(event: {
  message: {
    content: Array<{
      tool_use_id: string;
      type: string;
      content: string;
      is_error: boolean;
    }>;
  };
}): Message[] {
  if (!event.message?.content) return [];

  const messages: Message[] = [];
  for (const block of event.message.content) {
    if (block.type === "tool_result") {
      messages.push({
        role: "toolResult",
        toolCallId: block.tool_use_id,
        toolName: "", // CC doesn't echo tool name in results; filled by matching above toolCall
        content: block.is_error ? [{ type: "text", text: block.content }] : [],
        isError: block.is_error,
        timestamp: Date.now(),
      });
    }
  }
  return messages;
}

/**
 * Extract usage and cost from the final result event.
 */
function extractResultUsage(resultEvent: CCResultEvent): {
  input: number;
  output: number;
  cacheRead: number;
  cacheWrite: number;
  cost: number;
  contextTokens: number;
  turns: number;
} {
  const usage = resultEvent.usage ?? {};
  const modelUsage = resultEvent.modelUsage ?? {};

  // Aggregate from modelUsage if available (more accurate)
  let input = 0;
  let output = 0;
  let cacheRead = 0;
  let cacheWrite = 0;
  let cost = 0;

  for (const model of Object.values(modelUsage)) {
    input += model.inputTokens ?? 0;
    output += model.outputTokens ?? 0;
    cacheRead += model.cacheReadInputTokens ?? 0;
    cacheWrite += model.cacheCreationInputTokens ?? 0;
    cost += model.costUSD ?? 0;
  }

  // Fallback to top-level usage if modelUsage is empty
  if (cost === 0 && resultEvent.total_cost_usd) {
    cost = resultEvent.total_cost_usd;
  }
  if (input === 0) input = usage.input_tokens ?? 0;
  if (output === 0) output = usage.output_tokens ?? 0;
  if (cacheRead === 0) cacheRead = usage.cache_read_input_tokens ?? 0;
  if (cacheWrite === 0) cacheWrite = usage.cache_creation_input_tokens ?? 0;

  // Estimate context tokens from the largest model's context window
  let contextTokens = 0;
  for (const model of Object.values(modelUsage)) {
    if (model.contextWindow && model.contextWindow > contextTokens) {
      contextTokens = model.contextWindow;
    }
  }

  return {
    input,
    output,
    cacheRead,
    cacheWrite,
    cost,
    contextTokens,
    turns: resultEvent.num_turns ?? 0,
  };
}

// Claude Code Runner

export async function runClaudeCodeAgent(
  agent: AgentConfig,
  task: string,
  cwd: string,
  _parentModes: string[],
  _modeOverride: string | undefined,
  signal: AbortSignal | undefined,
  onUpdate: OnUpdateCallback | undefined,
  makeDetails: (results: SingleResult[]) => SubagentDetails,
  _ctx: ExtensionContext,
): Promise<SingleResult> {
  const currentResult = createPendingResult(agent.name, task);
  currentResult.model = agent.model;
  currentResult.runner = "claude-code";

  const emitUpdate = makeEmitUpdate(currentResult, onUpdate, makeDetails);

  let tmpPromptDir: string | null = null;
  let tmpPromptPath: string | null = null;

  try {
    // Write system prompt to temp file
    if (agent.systemPrompt.trim()) {
      const tmp = await writePromptToTempFile(agent.name, agent.systemPrompt);
      tmpPromptDir = tmp.dir;
      tmpPromptPath = tmp.filePath;
    }

    // Build CLI arguments
    const args: string[] = [
      "-p",
      `Task: ${task}`,
      "--output-format",
      "stream-json",
      "--verbose",
    ];

    if (agent.model) {
      args.push("--model", agent.model);
    }

    // Tool restrictions: --tools defines the available built-in set
    // (--allowedTools only controls auto-approval, not availability)
    const allowedTools = agent.tools;
    if (allowedTools && allowedTools.length > 0) {
      args.push("--tools", allowedTools.join(","));
    }

    // System prompt via temp file
    if (tmpPromptPath) {
      args.push("--append-system-prompt-file", tmpPromptPath);
    }

    // Permission mode: bypassPermissions for fully headless operation
    args.push("--permission-mode", "bypassPermissions");

    let wasAborted = false;
    let spawnErrorMessage: string | undefined;
    let resultEvent: CCResultEvent | undefined;

    const exitCode = await new Promise<number>((resolve) => {
      const invocation = getClaudeCodeInvocation(args);
      const proc = spawn(invocation.command, invocation.args, {
        cwd,
        shell: false,
        stdio: ["ignore", "pipe", "pipe"],
        env: {
          ...process.env,
        },
      });

      let stdoutBuffer = "";
      let killTimeout: ReturnType<typeof setTimeout> | undefined;
      let settled = false;

      const killProc = () => {
        wasAborted = true;
        currentResult.stopReason = "aborted";
        currentResult.errorMessage = "Subagent was aborted";
        proc.kill("SIGTERM");
        killTimeout = setTimeout(() => {
          if (!proc.killed) proc.kill("SIGKILL");
        }, 5000);
      };

      const cleanup = () => {
        if (killTimeout) clearTimeout(killTimeout);
        if (signal) signal.removeEventListener("abort", killProc);
      };

      const finish = (code: number) => {
        if (settled) return;
        settled = true;
        cleanup();
        resolve(code);
      };

      // Process each NDJSON line from Claude Code's stdout
      const processLine = (line: string) => {
        const event = parseCCLine(line);
        if (!event) return;

        switch (event.type) {
          case "system": {
            // Init event: extract model info if we don't have it
            if (event.subtype === "init" && !currentResult.model) {
              currentResult.model =
                (event as { model?: string }).model ?? agent.model;
            }
            // api_retry: log but don't treat as error
            break;
          }

          case "assistant": {
            const assistantEvent = event as CCAssistantEvent;
            const message = buildAssistantMessage(assistantEvent);
            if (message.content.length > 0) {
              currentResult.messages.push(message);
              currentResult.usage.turns++;

              // Accumulate per-message usage
              if (message.usage) {
                currentResult.usage.input += message.usage.input || 0;
                currentResult.usage.output += message.usage.output || 0;
                currentResult.usage.cacheRead += message.usage.cacheRead || 0;
                currentResult.usage.cacheWrite += message.usage.cacheWrite || 0;
              }

              if (!currentResult.model && message.model) {
                currentResult.model = message.model;
              }
              if (message.stopReason) {
                currentResult.stopReason = message.stopReason;
              }
            }
            emitUpdate();
            break;
          }

          case "user": {
            // Tool result messages — add to message stream for display
            const toolResultMsgs = buildToolResultMessages(
              event as {
                message: {
                  content: Array<{
                    tool_use_id: string;
                    type: string;
                    content: string;
                    is_error: boolean;
                  }>;
                };
              },
            );
            for (const msg of toolResultMsgs) {
              currentResult.messages.push(msg);
            }
            break;
          }

          case "result": {
            resultEvent = event as CCResultEvent;
            // Override usage with more accurate final data
            const finalUsage = extractResultUsage(resultEvent);
            currentResult.usage.input = finalUsage.input;
            currentResult.usage.output = finalUsage.output;
            currentResult.usage.cacheRead = finalUsage.cacheRead;
            currentResult.usage.cacheWrite = finalUsage.cacheWrite;
            currentResult.usage.cost = finalUsage.cost;
            currentResult.usage.contextTokens = finalUsage.contextTokens;
            currentResult.usage.turns = finalUsage.turns;

            if (resultEvent.is_error) {
              currentResult.stopReason = "error";
              currentResult.errorMessage =
                resultEvent.result || "Claude Code returned an error";
            } else {
              currentResult.stopReason = mapCCStopReason(
                resultEvent.stop_reason,
              );
            }

            if (resultEvent.modelUsage) {
              // Extract primary model from modelUsage keys
              const models = Object.keys(resultEvent.modelUsage);
              if (models.length > 0 && !currentResult.model) {
                // Use the model that had the most output tokens
                let bestModel = models[0];
                let bestOutput = 0;
                for (const [name, usage] of Object.entries(
                  resultEvent.modelUsage,
                )) {
                  if ((usage.outputTokens ?? 0) > bestOutput) {
                    bestOutput = usage.outputTokens ?? 0;
                    bestModel = name;
                  }
                }
                currentResult.model = bestModel;
              }
            }

            // If no assistant text was captured but the result has text,
            if (
              !resultEvent.is_error &&
              resultEvent.result &&
              !getFinalOutput(currentResult.messages)
            ) {
              currentResult.messages.push({
                role: "assistant",
                content: [{ type: "text", text: resultEvent.result }],
                api: "anthropic-messages",
                provider: "anthropic",
                model: currentResult.model ?? "unknown",
                usage: {
                  input: 0,
                  output: 0,
                  cacheRead: 0,
                  cacheWrite: 0,
                  totalTokens: 0,
                },
                stopReason: "stop",
                timestamp: Date.now(),
              });
            }

            emitUpdate();
            // Result is the definitive final event — give the process a short
            // grace period to exit cleanly, then force-kill to avoid hanging
            const exitTimer = setTimeout(() => {
              if (!proc.killed) {
                proc.kill("SIGTERM");
                setTimeout(() => {
                  if (!proc.killed) proc.kill("SIGKILL");
                }, 5000).unref();
              }
            }, 5000);
            exitTimer.unref();
            break;
          }

          case "rate_limit_event":
            // Informational — no action needed
            break;

          case "stream_event":
            // Partial message streaming — not used for display
            break;

          default:
            // Unknown event type — ignore
            break;
        }
      };

      proc.stdout.on("data", (data) => {
        stdoutBuffer += data.toString();
        const lines = stdoutBuffer.split("\n");
        stdoutBuffer = lines.pop() || "";
        for (const line of lines) processLine(line);
      });

      proc.stderr.on("data", (data) => {
        currentResult.stderr += data.toString();
      });

      proc.on("close", (code, childSignal) => {
        // Process any remaining buffer
        if (stdoutBuffer.trim()) processLine(stdoutBuffer);

        if (typeof code === "number") finish(code);
        else if (wasAborted || childSignal) finish(130);
        else finish(1);
      });

      proc.on("error", (error) => {
        spawnErrorMessage = `Failed to start claude for agent "${agent.name}": ${error.message}`;
        currentResult.errorMessage = spawnErrorMessage;
        currentResult.stderr += `${spawnErrorMessage}\n`;
        finish(1);
      });

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
      // If we got a result event with an error, use that message
      if (resultEvent?.is_error && resultEvent.result) {
        currentResult.errorMessage = resultEvent.result;
      } else {
        currentResult.errorMessage = `Claude Code process exited with code ${exitCode}`;
      }
    }
    // If no result event was parsed, the stream-json output was malformed
    if (!resultEvent && exitCode === 0) {
      currentResult.stopReason = "error";
      currentResult.errorMessage =
        "No result event received from Claude Code — stream output may have been malformed";
    }
    return currentResult;
  } finally {
    cleanupTempPrompt(tmpPromptDir, tmpPromptPath);
  }
}
