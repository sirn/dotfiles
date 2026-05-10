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

Skill naming policy:

- Skills are named for stable, user-facing tasks (for example `code-generate-tests`, `code-plan-api`, or `code-setup-flake`).
- Create separate skill directories when workflows have distinct triggers, outputs, or verification steps.
- Keep small local options such as scope, risk tolerance, or output detail inside a skill.
- `SKILL.subagent.md` is a harness-specific variant for the same task, not a separate task.
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

    # Single vendored skill exposed as `brave-search-bx`.
    brave-search = {
      path = "${pkgs.local.skill-brave-search}/skills";
      prefix = "brave-search";
      skills = [ "bx" ];
    };

    # Whole upstream repositories can be packaged and filtered at discovery time.
    agent-stuff = {
      path = "${pkgs.local.mitsuhiko-agent-stuff}/skills";
      skills = [ "tmux" ];
    };
  };
}
```

Each skill set is a directory whose immediate children are actual skill directories. Home Manager discovers those skills and renders each harness-specific layout from the same normalized skill list. If `prefix` is set, discovered skills are exposed as flat prefixed names like `brave-search-bx`; the rendered `SKILL.md` frontmatter `name` is rewritten to match. If `skills` is set to a non-empty list, only those source skill directory names are discovered from that skill set; the default empty list discovers all skills.

### Add a New Subagent

Subagents are reusable expert roles, not workflow entry points. Workflows live in skills; subagents provide a focused read-only lens that a subagent-capable skill can compose.

1. Create `home-manager/config/agents/subagents/<name>.md` with the role prompt
2. Create `home-manager/config/agents/subagents/<name>.nix` with the `agents.subagents.<name>` configuration
3. Import the new file from `home-manager/config/agents/subagents/default.nix`
4. Keep public defaults provider-neutral; configure private provider-qualified model overrides privately

Use `SKILL.subagent.md` to orchestrate existing roles for a workflow. Do not create a new subagent for every task-specific workflow.

## Subagent Taxonomy

Skills are workflows/SOPs. Subagents are expert roles/lenses that skills can compose:

- `scout`: local codebase reconnaissance, file and pattern discovery, and convention mapping.
- `researcher`: authoritative documentation, API, migration, error, and advisory research.
- `planner`: design, architecture, refactoring, API, and schema planning with tradeoffs.
- `reviewer`: correctness, security, convention, simplicity, and quality review using the lens requested by the task.
- `oracle`: high-confidence adjudication for ambiguous, conflicting, or high-impact decisions.

There is intentionally no `worker` subagent yet. Delegated writes need a separate safety model; the main agent remains responsible for mutations.

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
