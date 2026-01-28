---
name: claude-code-reference
description: Reference for calling the Claude Code agent from other agents.
---

## Claude Code Reference

Invoke the Claude Code agent for sub-tasks using structured JSON output.

## Capabilities
*   **Core Tools**: File operations (Read, Write, Edit, Glob, Grep), Bash execution (safe subset).
*   **Web**: `WebSearch`, `WebFetch`.
*   **MCP Servers**:
    *   `context7`: Documentation queries.
    *   `brave-search`: Web search (Brave).
*   **Specialty**: Excellent at planning, reasoning, and complex refactoring. Uses `opusplan` model.

### 1. Start a New Task
```bash
claude -p "Your specific prompt here" --output-format json
```
*   **Output:** Returns a JSON object containing `session_id` and the response.

### 2. Resume a Task
To continue a specific session:
```bash
claude -p "Follow-up prompt" --session-id <SESSION_UUID> --output-format json
```
*   **Note:** Always use the `session_id` from the previous response instead of `--continue`.

### 3. Review Changes
To request a code review, provide the diff in the prompt:
```bash
claude -p "Review the following changes:\n\n$(jj diff -s -r @-)\n\n$(jj diff -r @-)" --output-format json
```
*   **Workflow**:
    1.  If no diff is provided, assume `jj diff -s -r @-` then `jj diff -r @-`.
    2.  If specific files are targeted, use `jj diff -r @- -- <file>`.
    3.  **Do not make changes** during a review.

### Prompting Instructions
*   **Be Specific:** Explicitly state the goal, context, and required output format in your prompt.
*   **Permissions:** If you are confident in the safety of the request, you may use `--dangerously-skip-permissions` to avoid interactive prompts blocking execution (only if environment permits).
*   **Session Management:** Extract `session_id` from the JSON output and pass it explicitly to `--session-id` for all follow-up interactions.