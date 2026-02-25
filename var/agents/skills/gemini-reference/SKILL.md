---
name: gemini-reference
type: reference
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
mkdir -p .my/gemini-sessions
echo "*" > .my/.gitignore
echo "Your specific prompt here" > .my/gemini-sessions/task.md
gemini "Read .my/gemini-sessions/task.md" --output-format json
rm .my/gemini-sessions/task.md
```
*   **Output:** Returns a JSON object. Capture the `session_id` field for future turns.

### 2. Resume a Task
To continue a specific session (preferred):
```bash
echo "Follow-up prompt" > .my/gemini-sessions/followup.md
gemini "Read .my/gemini-sessions/followup.md" --resume <SESSION_UUID> --output-format json
rm .my/gemini-sessions/followup.md
```
*   **Note:** Use the UUID obtained from the `session_id` field of the previous response.

### 3. Review Changes
To request a code review, ask the agent to run the diff command:
```bash
mkdir -p .my/gemini-sessions
echo "Run 'jj diff -s -r @-' and 'jj diff -r @-' and review the output." > .my/gemini-sessions/review.md
gemini "Read .my/gemini-sessions/review.md" --output-format json
rm .my/gemini-sessions/review.md
```
*   **Workflow**:
    1.  If no diff is provided, instruct agent to run `jj diff -s -r @-` then `jj diff -r @-`.
    2.  If specific files are targeted, use `jj diff -r @- -- <file>`.
    3.  **Do not make changes** during a review.

### Prompting Instructions
*   **Be Specific:** You are running in a headless context. Provide full context, file paths, and clear constraints in every prompt.
*   **JSON Parsing:** The output is a single JSON object. Use a tool like `jq` to extract the `response` (text) and `session_id`.
*   **Session Management:** Always store and reuse the `session_id` to maintain context across a multi-turn task. Do not rely on "latest" if you have the ID.