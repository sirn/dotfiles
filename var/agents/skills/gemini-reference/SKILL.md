---
name: gemini-reference
description: Reference for calling the Gemini CLI agent from other agents.
---

## Gemini CLI Reference

Invoke the Gemini agent for sub-tasks using structured JSON output and explicit session management.

## Capabilities
*   **Core Tools**: `ReadFileTool`, `WriteFile`, `Edit`, `GlobTool`, `GrepTool`, `ShellTool` (safe subset).
*   **MCP Servers**:
    *   `context7`: Documentation queries.
    *   `brave-search`: Web search (Brave).
*   **Specialty**: General purpose, large context window.

### 1. Start a New Task
```bash
gemini "Your specific prompt here" --output-format json
```
*   **Output:** Returns a JSON object. Capture the `session_id` field for future turns.

### 2. Resume a Task
To continue a specific session (preferred):
```bash
gemini "Follow-up prompt" --resume <SESSION_UUID> --output-format json
```
*   **Note:** Use the UUID obtained from the `session_id` field of the previous response.

### 3. Review Changes
To request a code review, provide the diff in the prompt:
```bash
gemini "Review the following changes:\n\n$(jj diff -s -r @-)\n\n$(jj diff -r @-)" --output-format json
```
*   **Workflow**:
    1.  If no diff is provided, assume `jj diff -s -r @-` then `jj diff -r @-`.
    2.  If specific files are targeted, use `jj diff -r @- -- <file>`.
    3.  **Do not make changes** during a review.

### Prompting Instructions
*   **Be Specific:** You are running in a headless context. Provide full context, file paths, and clear constraints in every prompt.
*   **JSON Parsing:** The output is a single JSON object. Use a tool like `jq` to extract the `response` (text) and `session_id`.
*   **Session Management:** Always store and reuse the `session_id` to maintain context across a multi-turn task. Do not rely on "latest" if you have the ID.