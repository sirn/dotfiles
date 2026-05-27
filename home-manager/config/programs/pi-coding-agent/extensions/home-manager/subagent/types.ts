/**
 * Shared types and helpers for the subagent extension.
 *
 * Used by both the Pi RPC runner and the Claude Code runner,
 * as well as the main extension index for orchestration and rendering.
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { Message } from "@earendil-works/pi-ai";
import type { AgentToolResult } from "@earendil-works/pi-agent-core";
import { withFileMutationQueue } from "@earendil-works/pi-coding-agent";

// Agent Config

export interface AgentConfig {
  name: string;
  description: string;
  tools?: string[];
  model?: string;
  concurrency?: number;
  systemPrompt: string;
  runner?: "pi" | "claude-code";
}

// Result Types

export interface UsageStats {
  input: number;
  output: number;
  cacheRead: number;
  cacheWrite: number;
  cost: number;
  contextTokens: number;
  turns: number;
}

export interface SingleResult {
  agent: string;
  task: string;
  exitCode: number;
  messages: Message[];
  stderr: string;
  usage: UsageStats;
  model?: string;
  stopReason?: string;
  errorMessage?: string;
  stepIndex?: number;
  started?: boolean;
  autoRetrying?: boolean;
  compacting?: boolean;
}

export interface SubagentDetails {
  mode: "steps";
  results: SingleResult[];
  totalSteps?: number;
  totalAgents?: number;
}

// Pi RPC Event Types

export type RpcEvent =
  | { type: "message_start" }
  | { type: "message_delta" }
  | { type: "message_end"; message?: Message }
  | { type: "tool_execution_start" }
  | {
      type: "tool_execution_end";
      isError?: boolean;
      result?: { content?: string };
    }
  | { type: "turn_start" }
  | { type: "turn_end" }
  | { type: "agent_end"; messages?: Message[] }
  | {
      type: "response";
      id?: string;
      command?: string;
      success: boolean;
      error?: string;
    }
  | {
      type: "extension_ui_request";
      id: string;
      method: string;
      title?: string;
      message?: string;
      options?: string[];
      placeholder?: string;
      prefill?: string;
      notifyType?: "info" | "warning" | "error";
      statusKey?: string;
      statusText?: string;
      widgetKey?: string;
      widgetLines?: string[];
      text?: string;
    }
  | {
      type: "auto_retry_start";
      attempt: number;
      maxAttempts: number;
      delayMs: number;
      errorMessage: string;
    }
  | {
      type: "auto_retry_end";
      success: boolean;
      attempt: number;
      finalError?: string;
    }
  | { type: "compaction_start"; reason: string }
  | {
      type: "compaction_end";
      reason: string;
      result?: unknown;
      aborted: boolean;
      willRetry: boolean;
      errorMessage?: string;
    }
  | { type: string; [key: string]: unknown };

// Claude Code Stream-JSON Event Types

export interface CCSystemEvent {
  type: "system";
  subtype: "init" | "api_retry" | "plugin_install";
  session_id?: string;
  tools?: string[];
  model?: string;
  permissionMode?: string;
  cwd?: string;
  attempt?: number;
  max_retries?: number;
  retry_delay_ms?: number;
  error_status?: number;
  error?: string;
  [key: string]: unknown;
}

export interface CCAssistantEvent {
  type: "assistant";
  message: Message & { usage?: CCMessageUsage };
  session_id?: string;
  parent_tool_use_id?: string | null;
}

export interface CCUserEvent {
  type: "user";
  message: {
    role: "user";
    content: Array<{
      tool_use_id: string;
      type: "tool_result";
      content: string;
      is_error: boolean;
    }>;
  };
  session_id?: string;
  parent_tool_use_id?: string | null;
  tool_use_result?: {
    stdout?: string;
    stderr?: string;
    interrupted?: boolean;
  };
}

export interface CCResultEvent {
  type: "result";
  subtype: "success" | "error";
  is_error: boolean;
  result: string;
  session_id?: string;
  total_cost_usd?: number;
  duration_ms?: number;
  num_turns?: number;
  stop_reason?: string | null;
  usage?: {
    input_tokens?: number;
    output_tokens?: number;
    cache_read_input_tokens?: number;
    cache_creation_input_tokens?: number;
    server_tool_use?: { web_search_requests?: number; web_fetch_requests?: number };
    [key: string]: unknown;
  };
  modelUsage?: Record<
    string,
    {
      inputTokens?: number;
      outputTokens?: number;
      cacheReadInputTokens?: number;
      cacheCreationInputTokens?: number;
      costUSD?: number;
      contextWindow?: number;
      maxOutputTokens?: number;
    }
  >;
  api_error_status?: number | null;
}

export interface CCStreamEvent {
  type: "stream_event";
  subtype?: string;
  event?: {
    type?: string;
    delta?: { text?: string; type?: string };
    [key: string]: unknown;
  };
  [key: string]: unknown;
}

export interface CCRateLimitEvent {
  type: "rate_limit_event";
  rate_limit_info?: {
    status?: string;
    requests_remaining?: number;
    tokens_remaining?: number;
    resetsAt?: number;
    [key: string]: unknown;
  };
  [key: string]: unknown;
}

export type CCEvent =
  | CCSystemEvent
  | CCAssistantEvent
  | CCUserEvent
  | CCResultEvent
  | CCStreamEvent
  | CCRateLimitEvent
  | { type: string; [key: string]: unknown };

interface CCMessageUsage {
  input_tokens?: number;
  output_tokens?: number;
  cache_read_input_tokens?: number;
  cache_creation_input_tokens?: number;
  [key: string]: unknown;
}

// Runner Type

export type OnUpdateCallback = (
  partial: AgentToolResult<SubagentDetails>,
) => void;

export type AgentRunner = (
  agent: AgentConfig,
  task: string,
  cwd: string,
  parentModes: string[],
  signal: AbortSignal | undefined,
  onUpdate: OnUpdateCallback | undefined,
  makeDetails: (results: SingleResult[]) => SubagentDetails,
  ctx: unknown,
) => Promise<SingleResult>;

// Helper Functions

export function createEmptyUsage(): UsageStats {
  return {
    input: 0,
    output: 0,
    cacheRead: 0,
    cacheWrite: 0,
    cost: 0,
    contextTokens: 0,
    turns: 0,
  };
}

export function createPendingResult(
  agent: string,
  task: string,
  stepIndex?: number,
): SingleResult {
  return {
    agent,
    task,
    exitCode: -1,
    messages: [],
    stderr: "",
    usage: createEmptyUsage(),
    stepIndex,
  };
}

export function createErrorResult(
  agent: string,
  task: string,
  errorMessage: string,
  exitCode = 1,
  stderr = errorMessage,
  model?: string,
  messages: Message[] = [],
): SingleResult {
  return {
    agent,
    task,
    exitCode,
    messages,
    stderr,
    usage: createEmptyUsage(),
    model,
    errorMessage,
  };
}

export function isPendingResult(result: SingleResult): boolean {
  return result.exitCode === -1;
}

export function isSkippedResult(result: SingleResult): boolean {
  return result.stopReason === "skipped";
}

export function isFailedResult(result: SingleResult): boolean {
  return (
    !isPendingResult(result) &&
    (result.exitCode !== 0 ||
      result.stopReason === "error" ||
      result.stopReason === "aborted")
  );
}

export function getResultErrorMessage(result: SingleResult): string {
  return (
    result.errorMessage ||
    result.stderr.trim() ||
    getFinalOutput(result.messages) ||
    (result.stopReason ? `Stopped: ${result.stopReason}` : "(no output)")
  );
}

export function getFinalOutput(messages: Message[]): string {
  for (let i = messages.length - 1; i >= 0; i--) {
    const msg = messages[i];
    if (msg.role === "assistant") {
      for (const part of msg.content) {
        if (part.type === "text") return part.text;
      }
    }
  }
  return "";
}

export async function writePromptToTempFile(
  agentName: string,
  prompt: string,
): Promise<{ dir: string; filePath: string }> {
  const tmpDir = await fs.promises.mkdtemp(
    path.join(os.tmpdir(), "pi-subagent-"),
  );
  const safeName = agentName.replace(/[^\w.-]+/g, "_");
  const filePath = path.join(tmpDir, `prompt-${safeName}.md`);
  await withFileMutationQueue(filePath, async () => {
    await fs.promises.writeFile(filePath, prompt, {
      encoding: "utf-8",
      mode: 0o600,
    });
  });
  return { dir: tmpDir, filePath };
}
