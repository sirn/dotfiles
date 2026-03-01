---
name: gemini-reference
type: reference
description: Reference for calling the Gemini CLI agent from other agents.
---

## Gemini CLI Reference

Invoke the Gemini agent for sub-tasks using structured JSON output and explicit session management.

## When to Use Gemini

Use Gemini when you need:
- **Large context window** - Can process extensive codebases in one pass
- **General purpose tasks** - Good balance of capability across domains
- **Fast responses** - Flash models for quick analysis
- **Cost efficiency** - Lower cost per token for large inputs

## Capabilities
*   **Core Tools**: `ReadFileTool`, `WriteFile`, `Edit`, `GlobTool`, `GrepTool`, `ShellTool`.
*   **MCP Servers**:
    *   `context7`: Documentation queries.
    *   `brave-search`: Web search (Brave).
*   **Specialty**: General purpose, large context window.

## Calling from Another Agent

Use `gemini "prompt"` to spawn Gemini as a sub-agent:

```bash
# Delegate a documentation task
result=$(gemini "Generate API documentation for @src/api/" --output-format json)

# Extract the response and session_id for follow-up
response=$(echo "$result" | jq -r '.response')
session_id=$(echo "$result" | jq -r '.session_id')
```

### Output Formats

| Format | Description |
|--------|-------------|
| `text` (default) | Plain text output |
| `json` | Single JSON object with `response`, `stats`, `session_id` |
| `stream-json` | Streaming newline-delimited JSON (JSONL) events |

**JSON Response Fields:**
- `response`: (string) The model's final answer
- `stats`: (object) Token usage and API latency metrics
- `session_id`: (string) Session identifier for resuming
- `error`: (object, optional) Error details if request failed

**Streaming JSON Event Types:**
- `init`: Session metadata (session ID, model)
- `message`: User and assistant message chunks
- `tool_use`: Tool call requests with arguments
- `tool_result`: Output from executed tools
- `error`: Non-fatal warnings and system errors
- `result`: Final outcome with aggregated statistics

## Handling Blocked Actions

When Gemini needs approval for an action it wasn't configured to auto-approve, the command may pause or complete with an error. For agent delegation:

```bash
# Step 1: Initial attempt with conservative permissions
result=$(gemini "Fix the build errors" --output-format json --approval-mode default)

# Step 2: Check for errors
if echo "$result" | jq -e '.error' > /dev/null 2>&1; then
  error_msg=$(echo "$result" | jq -r '.error.message')
  echo "Action blocked: $error_msg"
  session_id=$(echo "$result" | jq -r '.session_id')

  # Step 3: Continue with auto_edit mode for file operations
  echo "Continue fixing build errors" > /tmp/continue.md
  result=$(gemini "Read /tmp/continue.md" \
    --resume "$session_id" \
    --output-format json \
    --approval-mode auto_edit)
fi

# Extract final response
final_response=$(echo "$result" | jq -r '.response')
```

## Session Management for Agent Delegation

### Starting a Task

```bash
mkdir -p .gemini/sessions
echo "*" > .gemini/.gitignore

# Write the task with full context
cat > .gemini/sessions/task.md << 'EOF'
Review @src/auth/ and identify:
1. Authentication vulnerabilities
2. Missing input validation
3. Improper error handling
EOF

# Spawn Gemini with scoped permissions
result=$(gemini "Read .gemini/sessions/task.md" \
  --output-format json \
  --approval-mode default)

# Capture session info
session_id=$(echo "$result" | jq -r '.session_id')
rm .gemini/sessions/task.md
```

### Resuming a Session

Continue a specific session when you have the `session_id`:

```bash
echo "Implement fixes for the top 3 issues you found" > .gemini/sessions/continue.md

result=$(gemini "Read .gemini/sessions/continue.md" \
  --resume "$session_id" \
  --output-format json \
  --approval-mode auto_edit)

rm .gemini/sessions/continue.md
```

**Resume Options:**

| Flag | Description |
|------|-------------|
| `--resume <id>` | Resume specific session by ID or UUID |
| `--resume "latest"` | Resume most recent session |
| `--resume 5` | Resume by index number |
| `--list-sessions` | List available sessions |

**Important:** Always use explicit `--resume` with session ID when calling from another agent.

## Permission Handling

By default, Gemini requests confirmation for actions that modify your system. When calling from another agent, use `--approval-mode`:

### `--approval-mode`

Set the approval mode for tool execution:

| Mode | Description |
|------|-------------|
| `default` | Prompt for permission on sensitive actions |
| `auto_edit` | Automatically approve file edits only |

```bash
# Safe for file editing tasks
gemini "Fix lint errors" --approval-mode auto_edit

# For analysis tasks (no modifications)
gemini "Analyze codebase" --approval-mode default
```

### `--sandbox`

Run in a sandboxed environment for safer execution:

```bash
# Sandbox for untrusted code
gemini "Run untrusted code" --sandbox
gemini "Analyze suspicious file" --sandbox --output-format json
```

### `--full-auto`

Shortcut for automation: sets `--approval-mode auto_edit` and `--sandbox workspace-write`.

```bash
gemini "Fix lint errors and run tests" --full-auto
```

## Agent Delegation Patterns

### Pattern 1: Analysis → Implementation

```bash
# Phase 1: Analysis (default approvals)
analyze_result=$(gemini "Explore @src/ to understand the codebase structure" \
  --output-format json --approval-mode default)

session_id=$(echo "$analyze_result" | jq -r '.session_id')

# Phase 2: Implementation (auto_edit for file changes)
impl_result=$(gemini "Implement a logging middleware" \
  --resume "$session_id" --output-format json \
  --approval-mode auto_edit)
```

### Pattern 2: Escalating Permissions

```bash
# Start conservative
result=$(gemini "Fix the bug" --output-format json --approval-mode default)

# Check for errors
if echo "$result" | jq -e '.error' > /dev/null 2>&1; then
  session_id=$(echo "$result" | jq -r '.session_id')
  # Re-run with broader permissions
  result=$(gemini "Continue fixing with edit permissions" \
    --resume "$session_id" --output-format json \
    --approval-mode auto_edit)
fi
```

### Pattern 3: Model Selection by Task

```bash
# Quick analysis - use flash
gemini "Summarize this file" --model flash --output-format json

# Complex reasoning - use pro
gemini "Design a distributed system architecture" --model pro --output-format json

# Balanced - use auto (default)
gemini "Implement feature X" --model auto --output-format json
```

## Code Review Delegation

To delegate code review to Gemini:

```bash
mkdir -p .gemini/sessions
echo "Run 'jj diff -s -r @-' and 'jj diff -r @-' and review the output." > .gemini/sessions/review.md

result=$(gemini "Read .gemini/sessions/review.md" \
  --output-format json \
  --approval-mode default)

rm .gemini/sessions/review.md
```

**Important:** Do not use `--approval-mode auto_edit` during reviews - keep it read-only.

## Additional Useful Flags

| Flag | Description |
|------|-------------|
| `--model`, `-m` | Model to use (auto/pro/flash/flash-lite) |
| `--output-format`, `-o` | Output format (text/json/stream-json) |
| `--sandbox`, `-s` | Run in sandbox |
| `--include-directories` | Add directories to workspace |
| `--extensions`, `-e` | Enable specific extensions |
| `--allowed-mcp-server-names` | Allow specific MCP servers |
| `--debug`, `-d` | Debug mode with verbose logging |

## Model Selection

| Alias | Description |
|-------|-------------|
| `auto` | **Default.** Resolves to preview model if enabled, else pro |
| `pro` | Complex reasoning tasks |
| `flash` | Fast, balanced for most tasks |
| `flash-lite` | Fastest for simple tasks |

## Piping Input

Feed data into Gemini using Unix pipes:

```bash
# Pipe a file
cat error.log | gemini "Explain why this failed"

# Pipe command output
git diff | gemini "Write a commit message for these changes"

# Combined with file references
gemini "Review @package.json and explain the dependencies"
```

## Best Practices for Agent Delegation

### Prompting
*   **Be Specific:** Provide clear goals, file paths, and constraints.
*   **Include Context:** Use `@path/to/file` to reference files explicitly.
*   **Headless Context:** Gemini can't see your context - include everything in the prompt.

### Permission Safety
*   **Start Conservative:** Use `--approval-mode default` initially.
*   **Escalate as Needed:** Check `.error` field and re-run with `auto_edit`.
*   **Sandbox Untrusted Code:** Always use `--sandbox` when running untrusted code.

### Session Management
*   **Always Extract session_id:** Capture it from JSON output.
*   **Check for Errors:** Always inspect the `.error` field in JSON response.
*   **Session per Task:** Use separate sessions for unrelated tasks.

### Model Selection
*   **flash**: Quick summaries, simple tasks
*   **pro**: Complex architecture, security reviews
*   **auto**: Let Gemini decide based on prompt

## Example: Complete Agent Delegation Workflow

```bash
#!/bin/bash

# 1. Create temp directory
mkdir -p tmp/.gemini && echo "*" > tmp/.gitignore

# 2. Write task with full context
cat > tmp/task.md << 'EOF'
Analyze the codebase and provide:
1. Architecture overview
2. Potential security issues
3. Performance bottlenecks

Focus on @src/ and @config/ directories.
EOF

# 3. Spawn Gemini with initial permissions (read-only)
result=$(gemini "Read tmp/task.md" \
  --output-format json \
  --approval-mode default)

# 4. Check for errors
if echo "$result" | jq -e '.error' > /dev/null 2>&1; then
  echo "Error: $(echo "$result" | jq -r '.error.message')"
  exit 1
fi

# 5. Extract results
response=$(echo "$result" | jq -r '.response')
session_id=$(echo "$result" | jq -r '.session_id')
stats=$(echo "$result" | jq -r '.stats')

# 6. Report findings to parent agent
echo "=== Analysis Complete ==="
echo "Session ID: $session_id"
echo "Token usage: $(echo "$stats" | jq -r '.total_tokens // "N/A"')"
echo ""
echo "$response"

# 7. Optional: Continue for implementation
# echo "Now implement the security fixes" > tmp/implement.md
# gemini "Read tmp/implement.md" --resume "$session_id" --approval-mode auto_edit ...

# 8. Clean up
rm -rf tmp
```

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success |
| `1` | General error or API failure |
| `42` | Input error (invalid prompt or arguments) |
| `53` | Turn limit exceeded |

## Structured Output with Schema

Request JSON output matching a JSON Schema:

```bash
# Create schema file
cat > /tmp/schema.json << 'EOF'
{
  "type": "object",
  "properties": {
    "project_name": { "type": "string" },
    "programming_languages": { "type": "array", "items": { "type": "string" } }
  },
  "required": ["project_name", "programming_languages"]
}
EOF

# Run with schema
gemini "Extract project metadata from @package.json" \
  --output-schema /tmp/schema.json \
  -o /tmp/result.json \
  --output-format json
```
