---
name: codex-reference
type: reference
description: Reference for calling the Codex CLI agent from other agents.
---

## Codex CLI Reference

Invoke the Codex agent for sub-tasks using structured JSONL output and explicit session management.

## When to Use Codex

Use Codex when you need:
- **High-speed code generation** - Fastest at writing and editing code
- **Code review and triage** - Excellent at analyzing diffs and suggesting fixes
- **Apply-patch workflows** - Good at implementing specific changes
- **Cost efficiency** - Lower cost for straightforward coding tasks

## Capabilities
* **Core Tools**: File system access (Workspace Write), Shell execution.
* **MCP Servers**:
  * `context7`: Documentation queries.
  * `brave-search`: Web search (Brave).
* **Specialty**: High-speed code generation and review. Good for "apply this change" tasks.

## Calling from Another Agent

Use `codex exec` to spawn Codex as a sub-agent:

```bash
# Delegate a coding task
result=$(codex exec "Implement user authentication" --json --full-auto)

# Parse JSONL output - first line has session_id
thread_id=$(echo "$result" | head -1 | jq -r '.thread_id')

# Extract final response from last item.completed event
final_response=$(echo "$result" | grep '"type":"item.completed"' | jq -s '.[-1].item.text')
```

### Output Formats

| Format | Description |
|--------|-------------|
| `text` (default) | Formatted text output to stdout |
| `json` | Newline-delimited JSON (JSONL) events |

**JSONL Event Types:**
- `thread.started` - Session metadata including `thread_id`
- `turn.started` - Agent turn begins
- `turn.completed` - Agent turn completes with usage stats
- `turn.failed` - Turn failed with error details
- `item.started` / `item.completed` - Tool execution events
- `error` - Non-fatal warnings and system errors

**Extracting Session ID:**
```bash
# First line contains thread_id
thread_id=$(head -1 output.jsonl | jq -r '.thread_id')
```

## Handling Blocked Actions

When Codex needs permissions it doesn't have, check the JSONL for `turn.failed` or errors, then resume with broader permissions:

```bash
# Step 1: Initial attempt with read-only sandbox
output=$(codex exec "Fix the build errors" --json --sandbox read-only)

# Step 2: Check for failures
if echo "$output" | grep -q '"type":"turn.failed"'; then
  error=$(echo "$output" | grep '"type":"turn.failed"' | jq -r '.error.message')
  echo "Action blocked: $error"

  # Extract thread_id
  thread_id=$(echo "$output" | head -1 | jq -r '.thread_id')

  # Step 3: Resume with workspace-write permissions
  output=$(echo "Continue fixing build errors" | codex exec - \
    --json \
    --sandbox workspace-write \
    --full-auto)
fi

# Extract final response
final_response=$(echo "$output" | grep '"type":"item.completed"' | jq -s '.[-1].item.text')
```

## Session Management for Agent Delegation

### Starting a Task

```bash
mkdir -p .codex/sessions
echo "*" > .codex/.gitignore

# Write the task with full context
cat > .codex/sessions/task.md << 'EOF'
Implement a rate limiter middleware:
1. Read @src/middleware/ to understand patterns
2. Follow the existing error handling style
3. Add tests following @src/middleware/__tests__/example.test.ts
EOF

# Spawn Codex with appropriate permissions
output=$(codex exec "Read .codex/sessions/task.md" --json --sandbox workspace-write)

# Capture session info from first line
thread_id=$(echo "$output" | head -1 | jq -r '.thread_id')
rm .codex/sessions/task.md
```

### Resuming a Session

Continue a previous session using the `thread_id`:

```bash
# Resume by thread_id
echo "Add rate limiting for POST endpoints too" | codex exec - \
  resume "$thread_id" \
  --json \
  --sandbox workspace-write

# Resume most recent session
codex exec resume --last \
  --json \
  --sandbox workspace-write \
  "Continue the implementation"
```

**Resume Options:**

| Flag | Description |
|------|-------------|
| `resume <THREAD_ID>` | Resume specific session |
| `resume --last` | Resume most recent session in current directory |
| `resume --last --all` | Resume most recent from any directory |
| `-i <path>` | Attach images to follow-up prompt |

## Permission and Safety

By default, `codex exec` runs in a **read-only sandbox**. Configure permissions:

### `--ask-for-approval`, `-a`

Control approval prompts:

| Mode | Description |
|------|-------------|
| `untrusted` | Prompt for untrusted commands |
| `on-request` | Prompt when agent requests |
| `never` | No prompts (for agent delegation) |

```bash
# For agent delegation - no prompts
codex exec --ask-for-approval never --sandbox workspace-write "fix lint errors"
```

### `--sandbox`, `-s`

Select sandbox policy:

| Mode | Description |
|------|-------------|
| `read-only` | **Default.** Cannot modify files or network |
| `workspace-write` | Can edit files in the workspace |
| `danger-full-access` | Full system access (use only in isolated environments) |

```bash
# Safe for file editing
codex exec --sandbox workspace-write "refactor code"

# Conservative for analysis
codex exec --sandbox read-only --ask-for-approval never "analyze code"
```

### `--full-auto`

Shortcut: sets `--ask-for-approval on-request` and `--sandbox workspace-write`.

```bash
codex exec --full-auto "fix lint errors and run tests"
```

## Agent Delegation Patterns

### Pattern 1: Exploration → Implementation

```bash
# Phase 1: Read-only exploration
explore_output=$(codex exec "Explore @src/ to understand patterns" \
  --json --sandbox read-only)

thread_id=$(echo "$explore_output" | head -1 | jq -r '.thread_id')

# Phase 2: Implementation with write permissions
echo "Implement feature following patterns found" | codex exec - \
  --json \
  --sandbox workspace-write \
  resume "$thread_id"
```

### Pattern 2: Permission Escalation

```bash
# Start conservative
output=$(codex exec "Fix the bug" --json --sandbox read-only)

# Check for failures
if echo "$output" | grep -q '"type":"turn.failed"'; then
  thread_id=$(echo "$output" | head -1 | jq -r '.thread_id')
  # Re-run with broader permissions
  output=$(echo "Continue fixing" | codex exec - \
    --json \
    --sandbox workspace-write \
    --ask-for-approval never \
    resume "$thread_id")
fi
```

### Pattern 3: Ephemeral Sessions

```bash
# Use --ephemeral for one-off tasks that don't need resuming
output=$(codex exec --ephemeral --json \
  --sandbox workspace-write \
  "Generate a README for this project")

# No thread_id to track, output is self-contained
response=$(echo "$output" | grep '"type":"item.completed"' | jq -s '.[-1].item.text')
```

## Structured Output

### JSON Schema Validation

Request JSON output matching a schema:

```bash
# Create schema
cat > /tmp/schema.json << 'EOF'
{
  "type": "object",
  "properties": {
    "project_name": { "type": "string" },
    "languages": { "type": "array", "items": { "type": "string" } }
  },
  "required": ["project_name", "languages"]
}
EOF

# Run with schema
output=$(codex exec "Extract project metadata" \
  --json \
  --output-schema /tmp/schema.json \
  -o /tmp/result.json)
```

## Code Review Delegation

To delegate code review to Codex:

```bash
mkdir -p .codex/sessions
echo "Run 'jj diff -s -r @-' and 'jj diff -r @-' and review." > .codex/sessions/review.md

output=$(codex exec "Read .codex/sessions/review.md" \
  --json \
  --sandbox read-only \
  --ask-for-approval never)

rm .codex/sessions/review.md
```

**Important:** Use `--sandbox read-only` for reviews - no modifications allowed.

## Additional Useful Flags

| Flag | Description |
|------|-------------|
| `--model`, `-m` | Override model (gpt-5-codex) |
| `--json` | JSONL output |
| `--ephemeral` | Don't persist session files |
| `--full-auto` | Automation preset |
| `-o <path>` | Write final message to file |
| `--output-schema <path>` | JSON Schema for output |
| `--cd`, `-C` | Set working directory |
| `--add-dir <path>` | Grant additional directories |
| `--config`, `-c` | Config override |
| `--profile`, `-p` | Config profile |
| `--skip-git-repo-check` | Run outside git repo |
| `--image`, `-i` | Attach images |

## Piping Input

```bash
# Pipe content
cat error.log | codex exec --json "Explain these errors"

# Read from stdin
codex exec - < prompt.txt --json
```

## Authentication in Agent Delegation

Set `CODEX_API_KEY` as environment variable:

```bash
export CODEX_API_KEY=<api-key>
codex exec --json "analyze code"
```

**Note:** `CODEX_API_KEY` is only supported in `codex exec`.

## Git Repository Requirement

Codex requires a Git repository by default. Override with:

```bash
codex exec --skip-git-repo-check "analyze standalone files"
```

## Best Practices for Agent Delegation

### Prompting
*   **Be Specific:** Provide clear, self-contained instructions.
*   **Reference Files:** Use `@path/to/file` for explicit file references.
*   **Include Context:** Codex can't see your context - include everything.

### Permission Safety
*   **Least Privilege:** Start with `read-only`, escalate to `workspace-write`.
*   **Auto-approve in Delegation:** Use `--ask-for-approval never` with restricted sandboxes.
*   **Full-auto Shortcut:** Use `--full-auto` for quick delegation.

### Session Management
*   **Extract thread_id:** Always capture from `thread.started` event (first line).
*   **Parse JSONL:** Use `grep` and `jq` to extract specific events.
*   **Ephemeral for One-offs:** Use `--ephemeral` when you don't need to resume.

### Cost Control
*   **Check Usage:** Look at `turn.completed` event for token usage.
*   **Ephemeral:** Saves disk space for short tasks.

## Example: Complete Agent Delegation Workflow

```bash
#!/bin/bash

# 1. Create temp directory
mkdir -p tmp/.codex && echo "*" > tmp/.gitignore

# 2. Write task with full context
cat > tmp/task.md << 'EOF'
Analyze @src/components/ and:
1. List all React components
2. Identify unused props
3. Suggest performance optimizations
EOF

# 3. Spawn Codex with read-only permissions
output=$(codex exec "Read tmp/task.md" \
  --json \
  --sandbox read-only \
  --ask-for-approval never)

# 4. Check for failures
if echo "$output" | grep -q '"type":"turn.failed"'; then
  echo "Error: $(echo "$output" | grep '"type":"turn.failed"' | jq -r '.error.message')"
  exit 1
fi

# 5. Extract results
thread_id=$(echo "$output" | head -1 | jq -r '.thread_id')
final_response=$(echo "$output" | grep '"type":"item.completed"' | jq -s '.[-1].item.text')
usage=$(echo "$output" | grep '"type":"turn.completed"' | jq -s '.[-1].usage')

# 6. Report to parent agent
echo "=== Analysis Complete ==="
echo "Thread ID: $thread_id"
echo "Usage: $(echo "$usage" | jq -r '.input_tokens // 0') input, $(echo "$usage" | jq -r '.output_tokens // 0') output"
echo ""
echo "$final_response"

# 7. Optional: Continue for implementation
# echo "Implement the top 3 optimizations" | codex exec - --json --sandbox workspace-write --full-auto resume "$thread_id"

# 8. Clean up
rm -rf tmp
```

## Parsing JSONL Output

Common patterns for extracting data from Codex JSONL:

```bash
# Extract thread_id (first line)
thread_id=$(head -1 output.jsonl | jq -r '.thread_id')

# Get all agent messages
echo "$output" | grep '"type":"item.completed"' | jq -r '.item.text'

# Get token usage
usage=$(echo "$output" | grep '"type":"turn.completed"' | jq -s '.[-1].usage')
input_tokens=$(echo "$usage" | jq -r '.input_tokens')

# Check for any errors
echo "$output" | grep '"type":"error"' | jq -r '.error.message'

# Filter specific item types (command_execution, file_change, etc.)
echo "$output" | grep '"type":"item.completed"' | jq 'select(.item.type == "file_change")'
```
