# AI Agent System Documentation

This directory contains the shared instruction system for multiple AI coding tools.

## Architecture

```
instruction.md   → Global rules (shared by all tools)
skills/          → SOP-style skill definitions (26 total)
agents/          → Sub-agent definitions for Claude Code + OpenCode
```

## How It Works

Each AI tool consumes these files differently:

| Tool | Receives | Consumption |
|------|----------|-------------|
| **Claude Code** | `instruction.md` + `skills/` + `agents/` | Skills in `~/.claude/skills/`, agents registered |
| **OpenCode** | `instruction.md` + `skills/` + `agents/` | Skills in `~/.config/opencode/skill/home-manager/`, agents registered |
| **Gemini CLI** | `instruction.md` + `skills/` | Skills in `~/.gemini/skills/`, no sub-agents |
| **Codex** | `instruction.md` + `skills/` | Skills in `~/.codex/skills/home-manager/`, no sub-agents |

## File Types

### SKILL.md vs SUBAGENT.md

- **SKILL.md**: Standard Operating Procedure to be executed directly by the current agent
- **SUBAGENT.md**: Enhanced version that delegates work to specialized sub-agents (only used when the host tool supports agents)

When both exist, the agent should prefer SUBAGENT.md for higher-quality parallel processing.

### Reference Skills

Skills with `type: reference` in their frontmatter are documentation-only:
- `claude-code-reference`
- `codex-reference`
- `gemini-reference`
- `jj-reference`
- `nix-reference`

These are for lookup only — do not "execute" them.

## Adding New Components

### Add a New Skill

1. Create `skills/<name>/SKILL.md` with frontmatter:
   ```yaml
   ---
   name: skill-name
   description: What this skill does
   ---
   ```
2. Optionally create `skills/<name>/SUBAGENT.md` for agent-based execution
3. Include any templates/examples in `skills/<name>/templates/` or `skills/<name>/examples/`

### Add a New Agent

1. Create `agents/<name>.md` with the agent prompt
2. Create `agents/<name>.toml` with configuration:
   ```toml
   description = "Agent description"

   [claude-code]
   allowedTools = ["Read", "Grep", "Glob", ...]
   color = "red"
   model = "sonnet"

   [opencode]
   model = "google/gemini-3-pro-preview"
   ```

## Agent Categories

- **Planners** (3): `opus-plan`, `gemini-plan`, `codex-plan` — Read-only planning agents
- **Researchers** (3): `code-researcher`, `code-debug-researcher`, `code-architect` — Analysis and research
- **Reviewers** (4): `quality-reviewer`, `convention-reviewer`, `simplicity-reviewer`, `security-researcher` — Analysis-only reviewers

## Configuration Per Tool

The Home Manager modules in `modules/programs/*.nix` handle deployment:

- `claude-code.nix`: Loads `instruction.md` as memory, skills to `~/.claude/skills/`, agents from `.toml`
- `opencode.nix`: Loads `instruction.md` as rules, skills to `~/.config/opencode/skill/home-manager/`, agents from `.toml`
- `gemini.nix`: Loads `instruction.md` as context, skills to `~/.gemini/skills/`
- `codex.nix`: Loads `instruction.md` as custom-instructions, skills to `~/.codex/skills/home-manager/`

## Maintenance Notes

- Keep tool-specific references out of `instruction.md`
- Avoid hardcoding model names in agent `.md` files (configure in `.toml`)
- Use generic language like "the user" instead of `$ARGUMENTS`
- When updating skills that check for project instructions, include all possible instruction filenames
