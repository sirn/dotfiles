---
name: tmux
description: Remote-control tmux sessions for interactive CLIs by sending keystrokes and capturing pane output. Use when a task requires an interactive terminal session (REPLs, TUI apps, coding agents). For non-interactive tasks, use the bash tool directly.
---

# tmux Skill

Use tmux when a command must stay interactive across multiple turns. For one-shot commands, background jobs, or simple stdout capture, use the bash tool directly.

## Session Detection and Reuse

Prefer reusing the current tmux session when you are already inside tmux. That keeps the workflow visible to the user and avoids extra server management.

```bash
if [ -n "$TMUX" ]; then
  SESSION=$(tmux display-message -p '#S')
  TARGET=$(tmux split-window -d -P -F '#{pane_id}' -t "$SESSION")
else
  SESSION=""
  TARGET=""
fi
```

- `tmux display-message -p '#S'` returns the current session name.
- `split-window -d -P -F '#{pane_id}'` creates a background pane and prints a stable pane ID you can reuse.
- If you want to keep using the current pane instead of creating a new one, use `TARGET=$(tmux display-message -p '#{pane_id}')`.

## Creating a New Session

If you are not already inside tmux, create a normal detached session and target its first pane.

```bash
if [ -z "$TARGET" ]; then
  SESSION="agent-$(date +%s)"
  tmux new-session -d -s "$SESSION"
  TARGET="${SESSION}:0.0"
fi
```

Useful follow-up patterns:

```bash
# Create another pane in the same session
SECOND_TARGET=$(tmux split-window -d -P -F '#{pane_id}' -t "$TARGET")

# Create a new window when the workflow deserves isolation
WINDOW_TARGET=$(tmux new-window -d -P -F '#{pane_id}' -t "$SESSION")
```

## Sending Input

Send literal text with `-l --`, then send `Enter` separately. This is more reliable for prompts and TUIs than sending one combined string.

```bash
text='echo hello from tmux'
tmux send-keys -t "$TARGET" -l -- "$text"
sleep 0.1
tmux send-keys -t "$TARGET" Enter
```

Control keys work as expected:

```bash
tmux send-keys -t "$TARGET" C-c
tmux send-keys -t "$TARGET" C-d
tmux send-keys -t "$TARGET" Escape
```

Guidelines:

- Use `send-keys -l --` for literal text so shells and TUIs receive exactly what you intend.
- Split text and `Enter` into separate calls for interactive applications.
- Add a short `sleep 0.1` before `Enter` when a TUI is sensitive to paste timing.

## Capturing Output

Read recent pane output with `capture-pane`.

```bash
tmux capture-pane -p -J -t "$TARGET" -S -200
```

- `-p` prints to stdout.
- `-J` joins wrapped lines, which makes parsing easier.
- `-S -200` captures the last 200 lines of scrollback. Increase it when you need more history.

## Waiting for Output / Prompts

Use `./wait-for-text.sh` to poll for a prompt, completion marker, or error message instead of open-coded loops.

```bash
./wait-for-text.sh -t "$TARGET" -p '>>> '
./wait-for-text.sh -t "$TARGET" -p 'Done' -F -T 30
./wait-for-text.sh -t "$TARGET" -p 'Error|Traceback' -T 5
```

Arguments:

- `-t <target>`: tmux target pane, window, or session
- `-p <pattern>`: regex by default
- `-F`: treat the pattern as a fixed string
- `-T <timeout>`: timeout in seconds
- `-i <interval>`: polling interval in seconds
- `-l <lines>`: number of scrollback lines to inspect

### Polling Strategy with Fibonacci Backoff

When waiting for long-running tasks, use a Fibonacci backoff pattern for polling intervals: start with short waits and gradually increase. Reset to the shortest interval once you detect the desired output.

**Option A: Multi-turn tool calls (preferred for agents and visibility)**

When interacting with coding agents (Claude, Codex, Gemini), multi-turn polling is strongly preferred because agents frequently output prompts requiring user interaction (permission requests, file selection, clarifying questions). A script that blocks internally would hide these prompts.

Instead, make multiple tool calls with increasing delays. This lets you see intermediate states and respond to agent prompts as they appear. See the **Multi-Prompt Agent Review Workflow** recipe below for a complete implementation.

Benefits:

- **See agent prompts immediately** - approve commands, answer questions
- **User can intervene** between turns if something looks wrong
- **Easier to debug** - each state is visible, not hidden in a loop

**Option B: Scripted polling with `wait-for-text.sh`**

For simple cases where you just need to wait for a known prompt and don't expect intermediate interactions:

```bash
./wait-for-text.sh -t "$TARGET" -p '>>> ' -i 2 -T 60
```

For cases where you must script the polling (e.g., in a shell script without multi-turn capability), implement Fibonacci backoff:

```bash
# Fibonacci sequence: 1, 1, 2, 3, 5, 8, 13...
intervals=(1 1 2 3 5 8 13)
for i in "${intervals[@]}"; do
  sleep "$i"
  output=$(tmux capture-pane -p -J -t "$TARGET" -S -50)
  if [[ "$output" == *"completion_marker"* ]]; then
    break
  fi
done
```

## Interactive Recipes

### Python REPL

Use `PYTHON_BASIC_REPL=1` so `send-keys` interacts with a plain prompt instead of readline-heavy behavior.

```bash
tmux send-keys -t "$TARGET" -l -- 'PYTHON_BASIC_REPL=1 python'
sleep 0.1
tmux send-keys -t "$TARGET" Enter
./wait-for-text.sh -t "$TARGET" -p '^>>> ' -T 10

tmux send-keys -t "$TARGET" -l -- 'print(2 + 2)'
sleep 0.1
tmux send-keys -t "$TARGET" Enter
./wait-for-text.sh -t "$TARGET" -p '^4$' -T 5
```

### Interactive `jj split`

Prefer non-interactive `jj split -r <id> -m ... -- <paths>` when that is enough. Use tmux when you intentionally need the interactive flow.

```bash
tmux send-keys -t "$TARGET" -l -- 'jj split'
sleep 0.1
tmux send-keys -t "$TARGET" Enter

# Inspect what jj or the spawned editor is asking for.
tmux capture-pane -p -J -t "$TARGET" -S -120

# Send the next keystrokes only after you have read the pane.
# Example responses depend on the exact prompt or editor state.
```

Treat the pane as the source of truth: capture output, decide the next keystroke, then continue.

### Orchestrating Coding Agents

Use tmux when the agent needs multi-turn interaction, prompt confirmation, or a full-screen TUI. For one-shot delegation, prefer the `claude-code-reference`, `codex-reference`, or `gemini-reference` skills.

**Simple agent task (no expected prompts):**

```bash
AGENT_TARGET=$(tmux split-window -d -P -F '#{pane_id}' -t "$TARGET")
tmux send-keys -t "$AGENT_TARGET" -l -- 'claude -p "Analyze @src/ for bugs"'
sleep 0.1
tmux send-keys -t "$AGENT_TARGET" Enter

# Wait for completion using scripted polling
./wait-for-text.sh -t "$AGENT_TARGET" -p '>' -T 60
output=$(tmux capture-pane -p -J -t "$AGENT_TARGET" -S -200)
```

**Complex agent workflow (continuous prompts expected):**

For agents that repeatedly ask for permission (e.g., Codex reviewing changes), use the **Multi-Prompt Agent Review Workflow** below. It handles:

- Capture-first detection of prompts
- Fibonacci backoff polling
- Scrollback growth as output accumulates
- Continuous prompt response cycles

The exact prompt pattern depends on the tool. Read the agent-specific reference skill before choosing the completion check.

### Multi-Prompt Agent Review Workflow

When reviewing changes with an agent (e.g., asking Codex to review your edits), expect continuous prompt cycles — the agent will repeatedly ask for command approval. Use capture-first polling with Fibonacci backoff and scrollback growth.

**Pattern: Capture first, never assume**

```bash
# 1. Send the review task
tmux send-keys -t "$TARGET" -l -- 'codex "Review these changes for accuracy"'
tmux send-keys -t "$TARGET" Enter

# 2. Initial capture after short wait — inspect what actually appeared
sleep 1
output=$(tmux capture-pane -p -J -t "$TARGET" -S -100)
echo "$output" | tail -20
# Look for: prompts, questions, "Would you like to run...", completion markers
```

**Pattern: Fibonacci multi-turn with prompt handling**

```bash
# Turn 1: Short wait, capture, check for prompt
sleep 1
output=$(tmux capture-pane -p -J -t "$TARGET" -S -100)
if [[ "$output" == *"Would you like to run"* ]]; then
  # Respond to prompt
  tmux send-keys -t "$TARGET" -l -- 'y'
  tmux send-keys -t "$TARGET" Enter
fi

# Turn 2: Reset to 1s after interaction, capture again
sleep 1
output=$(tmux capture-pane -p -J -t "$TARGET" -S -150)
# Check for next prompt or progress...

# Turn 3: Increase wait (Fibonacci: 1, 1, 2...)
sleep 2
output=$(tmux capture-pane -p -J -t "$TARGET" -S -200)

# Turn 4: Continue increasing (3s, 5s, 8s...)
sleep 3
output=$(tmux capture-pane -p -J -t "$TARGET" -S -300)
```

**Pattern: Scrollback growth**

Agent output accumulates. Double scrollback lines as needed:

```bash
# Early: small scrollback
output=$(tmux capture-pane -p -J -t "$TARGET" -S -100)

# After 1 minute: increase
output=$(tmux capture-pane -p -J -t "$TARGET" -S -300)

# After 3 minutes: increase more
output=$(tmux capture-pane -p -J -t "$TARGET" -S -800)
```

**Pattern: Completion detection**

Agents output section headers when done. Look for these markers:

| Agent/Task  | Look For                                 |
| ----------- | ---------------------------------------- |
| Code review | `"Findings"`, `"Summary"`, `"## Review"` |
| Coding task | Prompt return (`>`, `›`), `"Done"`       |
| Tests       | `"PASS"`, `"FAIL"`, exit codes           |
| Search      | `"Results:"`, prompt return              |

```bash
# Check for completion markers
if [[ "$output" == *"Findings"* ]] || [[ "$output" == *"Summary"* ]]; then
  echo "Review complete"
  break
fi
```

**Complete example: Codex review with continuous prompts**

```bash
# Setup
tmux send-keys -t "$TARGET" -l -- 'codex "Review var/agents/skills/*/SKILL.md for accuracy"'
tmux send-keys -t "$TARGET" Enter

# Fibonacci polling with prompt detection
intervals=(1 1 2 3 5 8 13 21)
scrollback=100
for i in "${intervals[@]}"; do
  sleep "$i"

  # Grow scrollback as output accumulates
  scrollback=$((scrollback + 100))
  output=$(tmux capture-pane -p -J -t "$TARGET" -S -$scrollback)

  # Handle permission prompts
  if [[ "$output" == *"Would you like to run"* ]] || [[ "$output" == *"Press enter to confirm"* ]]; then
    tmux send-keys -t "$TARGET" -l -- 'y'
    tmux send-keys -t "$TARGET" Enter
    # Reset: next iteration will start with sleep 1 again
    continue
  fi

  # Check for completion
  if [[ "$output" == *"Findings"* ]] && [[ "$output" == *"Summary"* ]]; then
    echo "$output" | tail -100
    break
  fi
done
```

Key principles:

- **Capture first** — inspect actual output before deciding next action
- **Respond immediately** — don't wait longer when a prompt is detected
- **Reset after interaction** — go back to short waits after sending input
- **Grow scrollback** — agent output accumulates over minutes
- **Look for headers** — section names indicate completion better than prompts

## Cleanup

Clean up only the panes or sessions you created for the task.

```bash
# Remove a helper pane
tmux kill-pane -t "$TARGET"

# Remove an entire session you created as a fallback
tmux kill-session -t "$SESSION"
```

Do not kill the user’s existing session just to tidy up a task.

## References

This skill is based on:

- **mitsuhiko/agent-stuff tmux skill** — https://github.com/mitsuhiko/agent-stuff/blob/main/skills/tmux/SKILL.md (MIT License)
- **steipete/clawdis tmux skill** — https://github.com/openclaw/skills/blob/main/skills/steipete/tmux/SKILL.md
- **tmux man page** — https://man7.org/linux/man-pages/man1/tmux.1.html
