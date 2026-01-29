---
name: codex-reference
description: Reference for calling the Codex CLI agent from other agents.
---

## Codex CLI Reference

Invoke the Codex agent for sub-tasks using structured JSONL output and explicit session management.

## Capabilities
*   **Core Tools**: File system access (Workspace Write), Shell execution.
*   **MCP Servers**:
    *   `context7`: Documentation queries.
    *   `brave-search`: Web search (Brave).
*   **Specialty**: High-speed code generation and review. Good for "apply this change" tasks.

### 1. Start a New Task
```bash
mkdir -p .my/codex-sessions
echo "*" > .my/.gitignore
echo "Your specific prompt here" > .my/codex-sessions/task.md
codex exec --json "Read .my/codex-sessions/task.md"
rm .my/codex-sessions/task.md
```
*   **Output:** Returns newline-delimited JSON (JSONL) events.
*   **Session ID:** The first line is typically `{"type":"thread.started","thread_id":"<UUID>"}`. Extract `thread_id` to use as the Session ID.

### 2. Resume a Task
To continue a specific session:
```bash
echo "Follow-up prompt" > .my/codex-sessions/followup.md
codex exec resume --json <SESSION_UUID> "Read .my/codex-sessions/followup.md"
rm .my/codex-sessions/followup.md
```

### 3. Review Changes
To request a code review, ask the agent to run the diff command:
```bash
mkdir -p .my/codex-sessions
echo "Run 'jj diff -s -r @-' and 'jj diff -r @-' and review the output." > .my/codex-sessions/review.md
codex exec --json "Read .my/codex-sessions/review.md"
rm .my/codex-sessions/review.md
```
*   **Workflow**:
    1.  If no diff is provided, instruct agent to run `jj diff -s -r @-` then `jj diff -r @-`.
    2.  If specific files are targeted, instruct agent to run `jj diff -r @- -- <file>`.
    3.  **Do not make changes** during a review.

### Prompting Instructions
*   **Be Specific:** Provide clear, self-contained instructions.
*   **Output Handling:** The output streams JSON events. You may need to read the stream or wait for completion. The final response is in the events.
*   **Session Management:** Always extract `thread_id` from the start of the conversation and reuse it. This ensures you are continuing the correct context.