# AI Agent System Documentation

This directory contains the shared instruction system for multiple AI coding tools.

## Architecture

```
AGENTS.md   → Global rules (shared by all tools)
skills/     → SOP-style skill definitions
agents/     → Sub-agent definitions for Claude Code + OpenCode
```

## How It Works

Each AI tool consumes these files differently:

| Tool            | Receives                            | Consumption                                              |
| --------------- | ----------------------------------- | -------------------------------------------------------- |
| **Claude Code** | `AGENTS.md` + `skills/` + `agents/` | Skills in `~/.claude/skills/`, agents registered         |
| **OpenCode**    | `AGENTS.md` + `skills/` + `agents/` | Skills in `~/.config/opencode/skill/`, agents registered |
| **Gemini CLI**  | `AGENTS.md` + `skills/`             | Skills in `~/.gemini/skills/`, no sub-agents             |
| **Codex**       | `AGENTS.md` + `skills/`             | Skills in `~/.codex/skills/`, no sub-agents              |

## File Types

### SKILL.md vs SKILL.subagent.md

- **SKILL.md**: Standard Operating Procedure to be executed directly by the current agent
- **SKILL.subagent.md**: Enhanced source-side variant for harnesses that support specialized sub-agents

Home Manager renders harness-specific skill trees. Subagent-capable harnesses receive a tree where `SKILL.subagent.md` is exposed as `SKILL.md` when present; other harnesses receive the original `SKILL.md`. The source-side `SKILL.subagent.md` file is hidden from rendered skill directories. Agents should always read `SKILL.md` from their configured skill directory.

Skills must be self-contained: do not invoke, call, or depend on another skill from within a skill. If a workflow needs behavior from another skill, inline the relevant procedure. `SKILL.subagent.md` may spawn configured sub-agents, but must not spawn or invoke skills.

### Reference Skills

Skills with `type: reference` in their frontmatter are documentation-only:

- `flake`
- `github-cli`
- `jujutsu`
- `nix`
- `terraform`

These are for lookup only — do not "execute" them.

## Adding New Components

### Add a New Skill

For first-party skills, add the skill under `skills/<name>/`:

1. Create `skills/<name>/SKILL.md` with frontmatter:
   ```yaml
   ---
   name: skill-name
   description: What this skill does
   ---
   ```
2. Optionally create `skills/<name>/SKILL.subagent.md` for subagent-capable harnesses
3. Include any templates/examples in `skills/<name>/templates/` or `skills/<name>/examples/`

For vendored skills, package the upstream repository under `pkgs/by-name/skill-*` and register it once in `home-manager/config/agents/skills.nix`:

```nix
{
  agents.skillSets = {
    home-manager = ../../../var/agents/skills;
    ast-grep = "${pkgs.local.skill-ast-grep}/skills";

    # Multi-skill bundles with generic skill names can be flattened with a prefix.
    brave-search = {
      path = "${pkgs.local.skill-brave-search}/skills";
      prefix = "brave-search";
    };

    # Whole upstream repositories can be packaged and filtered at discovery time.
    agent-stuff = {
      path = "${pkgs.local.mitsuhiko-agent-stuff}/skills";
      skills = [ "tmux" ];
    };
  };
}
```

Each skill set is a directory whose immediate children are actual skill directories. Home Manager discovers those skills and renders each harness-specific layout from the same normalized skill list. If `prefix` is set, discovered skills are exposed as flat prefixed names like `brave-search-web-search`; the rendered `SKILL.md` frontmatter `name` is rewritten to match. If `skills` is set to a non-empty list, only those source skill directory names are discovered from that skill set; the default empty list discovers all skills.

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

- **Researchers** (4): `code-researcher`, `code-debug-researcher`, `security-researcher`, `code-architect` — Analysis and research
- **Reviewers** (3): `quality-reviewer`, `convention-reviewer`, `simplicity-reviewer` — Analysis-only reviewers

## Configuration Per Tool

The Home Manager modules in `modules/programs/*.nix` handle deployment:

- `claude-code.nix`: Loads `AGENTS.md` as memory, skills to `~/.claude/skills/`, agents from `.toml`
- `opencode.nix`: Loads `AGENTS.md` as rules, skills to `~/.config/opencode/skill/`, agents from `.toml`
- `gemini.nix`: Loads `AGENTS.md` as context, skills to `~/.gemini/skills/`
- `codex.nix`: Loads `AGENTS.md` as custom-instructions, skills to `~/.codex/skills/`

## Maintenance Notes

- Keep tool-specific references out of `AGENTS.md`
- Avoid hardcoding model names in agent `.md` files (configure in `.toml`)
- Use generic language like "the user" instead of `$ARGUMENTS`
- Keep skills self-contained; inline shared procedures instead of asking the model to invoke another skill
- When updating skills that check for project instructions, include all possible instruction filenames
