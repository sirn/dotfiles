/**
 * Display/output utility functions for the subagent extension.
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { Message } from "@earendil-works/pi-ai";
import type { AgentToolResult } from "@earendil-works/pi-agent-core";
import {
  type ExtensionContext,
  withFileMutationQueue,
} from "@earendil-works/pi-coding-agent";

import type {
  OnUpdateCallback,
  SingleResult,
  SubagentDetails,
} from "./types.js";

// Strips ANSI/terminal escape sequences before displaying error summaries.
const ansiRegex =
  /(?:\x9B|\x1B\[)[0-9;:?]*[A-Za-z]|\x1B\][^\x07\x1B]*(?:\x07|\x1B\\)|\x1B[@-_]/g;

export function stripAnsi(str: string): string {
  return str
    .replace(ansiRegex, "")
    .replace(/\r|[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, "");
}

export function getResultErrorMessage(result: SingleResult): string {
  return (
    stripAnsi(result.errorMessage ?? "").trim() ||
    stripAnsi(result.stderr).trim() ||
    stripAnsi(getFinalOutput(result.messages)).trim() ||
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

export async function writeOutputToTempFile(output: string): Promise<string> {
  const tmpDir = await fs.promises.mkdtemp(
    path.join(os.tmpdir(), "pi-subagent-"),
  );
  const filePath = path.join(tmpDir, "output.txt");
  await withFileMutationQueue(filePath, async () => {
    await fs.promises.writeFile(filePath, output, {
      encoding: "utf-8",
      mode: 0o600,
    });
  });
  return filePath;
}

// Best-effort removal of a temp prompt file and its directory.
export function cleanupTempPrompt(
  dir: string | null,
  filePath: string | null,
): void {
  if (filePath) {
    try {
      fs.unlinkSync(filePath);
    } catch {
      /* ignore */
    }
  }
  if (dir) {
    try {
      fs.rmSync(dir, { recursive: true });
    } catch {
      /* ignore */
    }
  }
}

// Builds the progress callback shared by both runners: emits the result's
// latest text (or a running placeholder) alongside the live details.
export function makeEmitUpdate(
  currentResult: SingleResult,
  onUpdate: OnUpdateCallback | undefined,
  makeDetails: (results: SingleResult[]) => SubagentDetails,
): () => void {
  return () => {
    if (!onUpdate) return;
    onUpdate({
      content: [
        {
          type: "text",
          text: getFinalOutput(currentResult.messages) || "(running...)",
        },
      ],
      details: makeDetails([currentResult]),
    });
  };
}
