---
name: claude-code-reference
type: reference
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
mkdir -p .my/claude-code-sessions
echo "*" > .my/.gitignore
echo "Your specific prompt here" > .my/claude-code-sessions/task.md
claude -p "Read .my/claude-code-sessions/task.md" --output-format json
rm .my/claude-code-sessions/task.md
```
*   **Output:** Returns a JSON object containing `session_id` and the response.

### 2. Resume a Task
To continue a specific session:
```bash
echo "Follow-up prompt" > .my/claude-code-sessions/followup.md
claude -p "Read .my/claude-code-sessions/followup.md" --session-id <SESSION_UUID> --output-format json
rm .my/claude-code-sessions/followup.md
```
*   **Note:** Always use the `session_id` from the previous response instead of `--continue`.

### 3. Review Changes
To request a code review, ask the agent to run the diff command:
```bash
mkdir -p .my/claude-code-sessions
echo "Run 'jj diff -s -r @-' and 'jj diff -r @-' and review the output." > .my/claude-code-sessions/review.md
claude -p "Read .my/claude-code-sessions/review.md" --output-format json
rm .my/claude-code-sessions/review.md
```
*   **Workflow**:
    1.  If no diff is provided, instruct agent to run `jj diff -s -r @-` then `jj diff -r @-`.
    2.  If specific files are targeted, instruct agent to run `jj diff -r @- -- <file>`.
    3.  **Do not make changes** during a review.

### Prompting Instructions
*   **Be Specific:** Explicitly state the goal, context, and required output format in your prompt.
*   **Permissions:** If you are confident in the safety of the request, you may use `--dangerously-skip-permissions` to avoid interactive prompts blocking execution (only if environment permits).
*   **Session Management:** Extract `session_id` from the JSON output and pass it explicitly to `--session-id` for all follow-up interactions.