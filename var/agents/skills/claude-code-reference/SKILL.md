---
name: claude-code-reference
type: reference
description: Reference for calling the Claude Code agent from other agents.
---

## Claude Code Reference

Invoke the Claude Code agent for sub-tasks using structured JSON output and explicit session management.

## When to Use Claude Code

Use Claude Code when you need:
- **Complex reasoning and planning** - Uses `opusplan` model for sophisticated analysis
- **Multi-file refactoring** - Excellent at understanding and modifying across large codebases
- **Tool-rich operations** - File operations, Bash execution, Web search, MCP servers
- **Persistent sessions** - Continue complex tasks across multiple interactions

## Capabilities
*   **Core Tools**: File operations (Read, Write, Edit, Glob, Grep), Bash execution (safe subset).
*   **Web**: `WebSearch`, `WebFetch`.
*   **MCP Servers**:
    *   `context7`: Documentation queries.
    *   `brave-search`: Web search (Brave).
*   **Specialty**: Excellent at planning, reasoning, and complex refactoring. Uses `opusplan` model.

## Calling from Another Agent

Use `claude -p "prompt"` to spawn Claude as a sub-agent for delegated tasks:

```bash
# Delegate a complex analysis task
result=$(claude -p "Analyze the authentication flow in @src/auth/ and identify security issues" \
  --output-format json --allowedTools "Read,Glob,Grep")

# Extract the response and session_id for follow-up
response=$(echo "$result" | jq -r '.response')
session_id=$(echo "$result" | jq -r '.session_id')
```

### Output Formats

| Format | Description |
|--------|-------------|
| `text` (default) | Plain text output |
| `json` | Single JSON result with `response`, `session_id`, `usage`, `permission_denials` |
| `stream-json` | Real-time streaming JSON events |

**JSON Response Fields:**
- `response`: (string) The agent's final answer
- `session_id`: (string) UUID for continuing the session
- `usage`: (object) Token usage and cost information
- `permission_denials`: (array) Tools that were blocked and need approval

## Handling Permission Denials

When Claude needs tools you didn't pre-approve, the command completes but includes `permission_denials`. Continue the session with expanded permissions:

```bash
# Step 1: Initial attempt with limited permissions
result=$(claude -p "Fix the build errors" --output-format json --allowedTools "Read,Glob")

# Step 2: Check if permissions were denied
denials=$(echo "$result" | jq -r '.permission_denials')

if [[ $(echo "$denials" | jq 'length') -gt 0 ]]; then
  echo "Additional permissions needed: $denials"
  session_id=$(echo "$result" | jq -r '.session_id')

  # Step 3: Continue session with expanded permissions
  result=$(echo "Continue fixing build errors, you now have permission to run commands" \
    > /tmp/continue.md
  claude -p "Read /tmp/continue.md" \
    --session-id "$session_id" \
    --output-format json \
    --allowedTools "Read,Glob,Grep,Bash(npm *),Bash(node *),Edit")
fi

# Extract final response
final_response=$(echo "$result" | jq -r '.response')
```

## Session Management for Agent Delegation

### Starting a Task

```bash
mkdir -p .claude/sessions
echo "*" > .claude/.gitignore

# Write the task with full context
cat > .claude/sessions/task.md << 'EOF'
Analyze the database schema in @src/models/ and:
1. Identify missing indexes
2. Check for N+1 query patterns
3. Suggest optimizations
EOF

# Spawn Claude with scoped permissions
result=$(claude -p "Read .claude/sessions/task.md" \
  --output-format json \
  --allowedTools "Read,Glob,Grep")

# Capture session info
session_id=$(echo "$result" | jq -r '.session_id')
rm .claude/sessions/task.md
```

### Resuming a Session

Continue a specific session when you have the `session_id`:

```bash
echo "Based on your analysis, implement the top 3 optimizations" > .claude/sessions/continue.md

result=$(claude -p "Read .claude/sessions/continue.md" \
  --session-id "$session_id" \
  --output-format json \
  --allowedTools "Read,Edit,Bash(git *)")

rm .claude/sessions/continue.md
```

**Important:** Always use explicit `--session-id` instead of `--continue` when calling from another agent.

## Permission Handling

By default, Claude Code requests permission for actions that modify your system. When calling from another agent, pre-approve specific tools:

### `--allowedTools`

Specify which tools Claude can use without prompting:

```bash
# Read-only analysis
claude -p "Analyze codebase" --allowedTools "Read,Glob,Grep"

# Code editing with git
claude -p "Fix lint errors" --allowedTools "Edit,Bash(git status),Bash(git diff *),Bash(git add *),Bash(git commit *)"

# Nix operations
claude -p "Update package hashes" --allowedTools "Read,Edit,Bash(nix-prefetch-*),Bash(nix build *)"

# Permission rule syntax:
# - "Read" - allows all Read operations
# - "Bash(git *)" - allows Bash commands starting with "git "
# - Separate multiple with commas
```

### Permission Modes

Use `--permission-mode` for high-level permission behavior:

| Mode | Description |
|------|-------------|
| `default` | Prompt for permission on sensitive actions |
| `acceptEdits` | Automatically accept file edits |
| `plan` | Start in Plan Mode (read-only exploration) |
| `dontAsk` | Don't ask for permissions (still checks) |
| `bypassPermissions` | Skip all permission checks (use with caution) |

## Agent Delegation Patterns

### Pattern 1: Initial Exploration → Implementation

```bash
# Phase 1: Exploration (read-only)
explore_result=$(claude -p "Explore @src/ to understand the codebase structure" \
  --output-format json --allowedTools "Read,Glob")

session_id=$(echo "$explore_result" | jq -r '.session_id')

# Phase 2: Implementation (with edit permissions)
impl_result=$(echo "Implement a logging middleware following the patterns you found" \
  | claude -p "$(cat)" --session-id "$session_id" --output-format json \
  --allowedTools "Read,Write,Edit")
```

### Pattern 2: Permission Escalation

```bash
# Start conservative
result=$(claude -p "Fix the bug" --output-format json --allowedTools "Read")

# Check what was denied
denied=$(echo "$result" | jq -r '.permission_denials[] | select(.tool_name == "Bash")')

if [[ -n "$denied" ]]; then
  session_id=$(echo "$result" | jq -r '.session_id')
  # Re-run with broader permissions
  result=$(claude -p "Continue fixing with command execution allowed" \
    --session-id "$session_id" --output-format json \
    --allowedTools "Read,Bash(*),Edit")
fi
```

### Pattern 3: Parallel Sub-Agents

```bash
# Spawn multiple Claude sessions for different tasks
result1=$(claude -p "Analyze security" --output-format json &)
result2=$(claude -p "Analyze performance" --output-format json &)
result3=$(claude -p "Check test coverage" --output-format json &)

wait
# Combine results...
```

## Code Review Delegation

To delegate code review to Claude:

```bash
mkdir -p .claude/sessions
echo "Run 'jj diff -s -r @-' and 'jj diff -r @-' and review the output." > .claude/sessions/review.md

result=$(claude -p "Read .claude/sessions/review.md" \
  --output-format json \
  --allowedTools "Bash(jj *)")

rm .claude/sessions/review.md
```

**Important:** Do not allow Edit/Write tools during reviews - keep it read-only.

## Additional Useful Flags

| Flag | Description |
|------|-------------|
| `--model` | Set model (sonnet/opus/haiku) |
| `--max-turns` | Limit agentic turns before stopping |
| `--max-budget-usd` | Max spend before stopping |
| `--tools` | Restrict available tools (vs allow them) |
| `--verbose` | Full turn-by-turn output |
| `--no-session-persistence` | Don't save to disk (one-off tasks) |
| `--system-prompt` | Replace system prompt |
| `--append-system-prompt` | Append to system prompt |

## Best Practices for Agent Delegation

### Prompting
*   **Be Specific:** Provide clear goals, file paths, and constraints.
*   **Include Context:** The spawned agent can't see your context - include relevant files with `@path`.
*   **Provide Verification:** Include success criteria so Claude can verify its work.

### Permission Safety
*   **Start Conservative:** Use minimal `--allowedTools` initially.
*   **Escalate as Needed:** Check `permission_denials` and continue with broader permissions.
*   **Never use `--dangerously-skip-permissions`**: Always use scoped `--allowedTools`.

### Session Management
*   **Always Extract session_id:** Capture it from JSON output even if you don't plan to continue.
*   **Session per Task:** Use separate sessions for unrelated tasks.
*   **Check permission_denials:** Always inspect this array in the JSON response.

### Cost Control
*   **Use `--max-budget-usd`:** Prevent runaway costs in automation.
*   **Use `--max-turns`:** Limit how long the agent runs.
*   **Monitor usage:** Check the `usage` field in JSON output.

## Example: Complete Agent Delegation Workflow

```bash
#!/bin/bash

# 1. Create temp directory
mkdir -p tmp/.claude && echo "*" > tmp/.gitignore

# 2. Write task with full context
cat > tmp/task.md << 'EOF'
Analyze the codebase and provide:
1. Architecture overview
2. Potential security issues
3. Performance bottlenecks

Focus on @src/ and @config/ directories.
EOF

# 3. Spawn Claude with initial permissions
result=$(claude -p "Read tmp/task.md" \
  --output-format json \
  --allowedTools "Read,Glob,Grep" \
  --max-budget-usd 2.00)

# 4. Check for permission issues
denials=$(echo "$result" | jq '.permission_denials')
if [[ $(echo "$denials" | jq 'length') -gt 0 ]]; then
  echo "Note: Some tools were denied: $(echo "$denials" | jq -r '.[].tool_name')"
fi

# 5. Extract results
response=$(echo "$result" | jq -r '.response')
session_id=$(echo "$result" | jq -r '.session_id')
usage=$(echo "$result" | jq -r '.usage')

# 6. Report findings to parent agent
echo "=== Analysis Complete ==="
echo "Session ID: $session_id"
echo "Usage: $usage"
echo ""
echo "$response"

# 7. Optional: Continue for implementation
# echo "Now implement the security fixes" > tmp/implement.md
# claude -p "Read tmp/implement.md" --session-id "$session_id" ...

# 8. Clean up
rm -rf tmp
```
