---
name: handoff
description: Summarize the current session for handing off to another session. Use when the user wants to end the current session and transfer context to a new session.
---

Create a handoff summary for transferring the current session context to another session.

## Process

1. **Gather context**:
   - Review conversation history to identify what was being worked on
   - Note the current working directory and project context
   - Identify any running processes, servers, or background tasks
   - List uncommitted modified files (use `jj diff -s` or `git status` as appropriate)
   - Capture any specific user preferences, constraints, or instructions established during the session
   - Note any recent error messages, test failures, or stack traces if debugging

2. **Identify command patterns & skills**:
   - Note any project-specific commands used (build, test, lint, etc.)
   - Record any custom scripts or aliases utilized
   - Document any custom agent skills used during the session (e.g., `jujutsu`, `clickup`, `context7`)

3. **Determine remaining work**:
   - List incomplete tasks or TODOs mentioned in the conversation
   - Identify specific files the next agent should read immediately to gain context
   - Identify next steps or planned actions
   - Note any blockers or issues that need resolution

4. **Determine output location**:
   - Default: write to `HANDOFF.md` in the current directory
   - If user specifies a different location, use that instead

5. **Write/update the handoff file**:
   - If `HANDOFF.md` (or user-specified path) already exists, read it first
   - Append new handoff information with a timestamp and session separator
   - Do not overwrite existing content; update by prepending or appending

## Output Format

The handoff file should follow this structure:

```markdown
# Handoff - <Timestamp>

## Current Context

- Working directory: `<path>`
- Project: `<project name and brief description>`
- Uncommitted changes: `<summary of jj diff -s>`

## What We Were Working On

<Summary of the task, feature, bug fix, or investigation in progress>

## Files to Read First

- `<path/to/file1>` - <why it's relevant>
- `<path/to/file2>` - <why it's relevant>

## Key Context & Decisions

- <Important decisions made or context discovered>
- <User constraints or preferences established>
- <Environment state worth preserving (e.g., running dev servers)>

## Recent Errors / Debugging State

- <Exact error message or test failure being investigated, or "None">

## Command Patterns & Skills Used

- `<command>` - <what it's for>
- `<skill-name>` - <custom skills the next agent should be aware of>

## What's Left to Do

- [ ] <Incomplete task 1>
- [ ] <Incomplete task 2>
- [ ] <Next planned step>

## Blockers/Issues

- <Any blockers encountered, or "None">

---
```

## Output Location

Write to `HANDOFF.md` in the current working directory unless the user specifies a different path.

- Always check if the file already exists before writing
- If updating, preserve existing handoffs and add a new section with current timestamp
- Keep summaries concise but include enough context for a new session to pick up seamlessly
- Do not include sensitive information (passwords, API keys, tokens)
