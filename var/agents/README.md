# AI Agent System Documentation

This directory contains the shared instruction system for multiple AI coding tools.

## Architecture

```
AGENTS.md   → Global rules (shared by all tools)
skills/     → SOP-style workflow definitions
agents/     → Expert-role subagent definitions
```

## How It Works

Each AI tool consumes these files differently:

| Tool            | Receives                            | Consumption                                        |
| --------------- | ----------------------------------- | -------------------------------------------------- |
| **Pi**          | `AGENTS.md` + `skills/` + `agents/` | Skills in `~/.pi/agent/skills/`, agents registered |
| **Claude Code** | `AGENTS.md` + `skills/` + `agents/` | Skills in `~/.claude/skills/`, agents registered   |
| **Gemini CLI**  | `AGENTS.md` + `skills/`             | Skills in `~/.gemini/skills/`, no subagents        |
| **Codex**       | `AGENTS.md` + `skills/`             | Skills in `~/.codex/skills/`, no subagents         |

## File Types

### SKILL.md

- **SKILL.md**: Standard Operating Procedure for the skill. May reference and spawn specialized sub-agents.

Skills must be self-contained: do not invoke, call, or depend on another skill from within a skill. If a workflow needs behavior from another skill, inline the relevant procedure. Skills may spawn configured sub-agents, but must not spawn or invoke other skills.

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

- Expose only canonical task-specific skill names; add aliases only when the alias is itself a supported workflow.

For vendored skills, package the upstream repository under `pkgs/by-name/skill-*` and register it once in `home-manager/config/agents/skills.nix`:

```nix
{
  agents.skillSets = {
    apis = ../../../var/agents/skills/apis;
    coding = ../../../var/agents/skills/coding;
    lifecycle = ../../../var/agents/skills/lifecycle;
    references = ../../../var/agents/skills/references;

    ast-grep = "${pkgs.local.skill-ast-grep}/skills";


    # Whole upstream repositories can be packaged and filtered at discovery time.
    agent-stuff = {
      path = "${pkgs.local.mitsuhiko-agent-stuff}/skills";
      skills = [ "tmux" ];
    };
  };
}
```

First-party skills (e.g., `apis/`, `coding/`, `lifecycle/`, `references/`) are auto-discovered with names matching their directory. Vendored skills use `prefix` to expose specific skill names from upstream repos (e.g., `prefix = "ast-grep"` with `skills = [ "default" ]` yields `ast-grep-default`). The rendered `SKILL.md` frontmatter `name` is rewritten to match. If `skills` is set to a non-empty list, only those source skill directory names are discovered from that skill set; the default empty list discovers all skills.

### Add a New Subagent

Subagents are reusable expert roles, not workflow entry points. Workflows live in skills; subagents provide a focused read-only lens that a subagent-capable skill can compose.

1. Create `home-manager/config/agents/subagents/<name>.md` with the role prompt
2. Create `home-manager/config/agents/subagents/<name>.nix` with the `agents.subagents.<name>` configuration
3. Import the new file from `home-manager/config/agents/subagents/default.nix`
4. Keep public defaults provider-neutral; configure private provider-qualified model overrides privately

Skills may orchestrate existing subagent roles for a workflow. Do not create a new subagent for every task-specific workflow.

## Subagent Taxonomy

Skills are workflows/SOPs. Subagents are expert roles/lenses that skills can compose:

- `scout`: local codebase reconnaissance, file and pattern discovery, and convention mapping.
- `researcher`: authoritative documentation, API, migration, error, and advisory research.
- `planner`: design, architecture, refactoring, API, and schema planning with tradeoffs.
- `reviewer`: correctness, security, convention, simplicity, and quality review using the lens requested by the task.
- `oracle`: high-confidence adjudication for ambiguous, conflicting, or high-impact decisions.
- `worker`: focused implementation of code and configuration changes delegated by the orchestrator.

The orchestrator delegates write operations to `worker` to keep its own context free for quality control. The orchestrator remains responsible for reviewing worker output and verifying changes before finalizing.

## Configuration Per Tool

The Home Manager modules in `modules/programs/*.nix` handle deployment:

- `pi-coding-agent.nix`: Loads `AGENTS.md`, skills to `~/.pi/agent/skills/`, and agents to `~/.pi/agent/agents/`
- `claude-code.nix`: Loads `AGENTS.md` as memory, skills to `~/.claude/skills/`, and agents to Claude Code
- `gemini.nix`: Loads `AGENTS.md` as context, skills to `~/.gemini/skills/`
- `codex.nix`: Loads `AGENTS.md` as custom-instructions, skills to `~/.codex/skills/`

## Maintenance Notes

- Keep tool-specific references out of `AGENTS.md`
- Avoid hardcoding provider-qualified model names in public agent files; configure private overrides privately
- Use generic language like "the user" instead of `$ARGUMENTS`
- Keep skills self-contained; inline shared procedures instead of asking the model to invoke another skill
- When updating skills that check for project instructions, include all possible instruction filenames
