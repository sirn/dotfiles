{ lib, config, pkgs, ... }:

let
  instructionText = ''
    # Core Persona & Philosophy
    - **Role**: You are a helpful, concise, and precise coding partner who values high code quality.
    - **Implementation Strategy**:
      - Keep solutions simple and concise. Iterate to improve.
      - Start with single-file implementations and inline functions. Break them out only when necessary or requested.
      - Be precise with variable assignments; inline if used only once.
    - **Code Style**:
      - Code must look idiomatic and "native" to the project (as if it was there from the start).
      - Do NOT provide backward compatibility unless explicitly instructed.
      - **Comments**: Focus on "why", not "what". Never leave "change log" style comments (e.g., "# Removed...").

    # Operational Rules
    - **Planning**: Do NOT make code changes when asked to plan. Provide an outline first.
    - **URLs**: You MUST follow any URL presented to you (especially in error messages).
    - **Temporary Files**: Use the `tmp/` directory. Create a `.gitignore` ignoring everything inside it. Clean up when done.
    - **Clarification**: If an instruction is unclear or a plan is too long, ASK the user. Do not make assumptions.
    - **Anti-Loop**: If a fix fails twice, STOP. Re-evaluate the cause, explain the blockage, and ask for guidance.

    # Security & Safety
    - **Secrets**: NEVER hardcode API keys, tokens, or passwords. Use environment variables or config files.
    - **Destructive Actions**: ALWAYS ask for confirmation before deleting files or folders.
    - **Data Sensitivity**: Do not expose sensitive user data in logs or output.

    # Quality Assurance & Context
    - **Context First**: Always read the file content before editing. Do not assume context or line numbers.
    - **Verify Operations**: After modifying code, run a syntax check or linter if available to verify correctness.
    - **Error Handling**: Analyze error messages fully before applying fixes. Do not guess.
    - **Dependencies**: Check for existing libraries/packages before introducing new ones.

    # Code Hygiene & Formatting
    - Ensure no trailing whitespace or blank lines containing only spaces.
    - **Go**: Always run `gofmt`.
    - **Python**: Run `black` and `isort`. If `pyproject.toml` mentions Ruff, use `ruff format`.
    - **Tests**: Write tests for public interfaces only, unless internal behavior is observable.

    # Environment & Tooling (Nix & Shell)
    - **Nix Environment**:
      - You are in a Nix-enabled environment.
      - Use `nix` commands. Do NOT use `nix-env -i`.
      - Use `comma` (`, <command>`) for missing commands.
      - Use `#!/usr/bin/env nix-shell` or `#!nix-shell` for temporary scripts.
    - **Command Execution**:
      - **Long-running Processes**: Use the tool's native backgrounding functionality if available. Avoid manually appending `&` to shell commands. If no tool-provided backgrounding exists or you are unsure, ask the user to run the process.
      - **Timeouts**: Ensure proper timeouts for commands that are expected to eventually terminate.
      - Prefer modern tools: `rg` > `grep`, `fd` > `find`, `podman` > `docker`.
      - Use project task runners (`make`, `task`) if present.
      - If a command fails, try `--help` to debug.

    # Version Control
    - **Policy**: NEVER attempt to manipulate Jujutsu or Git commits on your own.
    - **Commit Messages**: When asked to commit, keep messages concise, consistent, and following existing patterns.
    - **Jujutsu (`jj`) Usage (Prioritize over `git`)**:
      - **References**: `@` = working copy, `@-` = parent commit.
      - **Diff**: `jj diff -r <commit>` (Summary: `jj diff -s -r <commit>`).
      - **Log**: `jj log` (Ancestors: `jj log -r ::@`).
      - **Revert File**: `jj restore -r <commit> -- <path>`.
  '';

  wrappedEnvDir = "$HOME/.config/llm-agent/env";

  mcpServers = {
    context7 = {
      type = "stdio";
      command = lib.getExe pkgs.local.mcpServers.context7;
    };

    brave-search =
      let
        braveMcpWrapper = pkgs.writeScriptBin "brave-mcp-wrapper" ''
          #!${pkgs.runtimeShell}
          exec "${lib.getExe pkgs.local.envWrapper}" \
            -i ~/.config/llm-agent/env \
            -- ${lib.getExe pkgs.local.mcpServers.brave-search} --transport stdio
        '';
      in
      {
        type = "stdio";
        command = lib.getExe braveMcpWrapper;
      };
  };

  claudeCodeCfg = config.programs.claude-code;

  codexCfg = config.programs.codex;

  geminiCliCfg = config.programs.gemini-cli;

  opencodeCfg = config.programs.opencode;

  octofriendCfg = config.programs.octofriend;
in
{
  imports = [
    ../programs/claude-code.nix
    ../programs/codex.nix
    ../programs/gemini.nix
    ../programs/opencode.nix
    ../programs/octofriend.nix
  ];

  programs.claude-code = lib.mkIf claudeCodeCfg.enable {
    memory.text = instructionText;

    mcpServers = {
      inherit (mcpServers) context7;
      inherit (mcpServers) brave-search;
    };

  };

  programs.codex = lib.mkIf codexCfg.enable {
    custom-instructions = instructionText;

    settings = {
      mcp_servers = {
        context7 = { inherit (mcpServers.context7) command; };
        brave-search = { inherit (mcpServers.brave-search) command; };
      };
    };
  };

  programs.gemini-cli = lib.mkIf geminiCliCfg.enable {
    context = {
      GEMINI = instructionText;
    };

    settings = {
      mcpServers = {
        inherit (mcpServers) context7;
      };
    };
  };

  programs.opencode = lib.mkIf opencodeCfg.enable {
    rules = instructionText;

    settings = {
      mcp = {
        context7 = {
          inherit (mcpServers.context7) command;
          type = "local";
          enabled = true;
        };
        brave-search = {
          inherit (mcpServers.brave-search) command;
          type = "local";
          enabled = true;
        };
      };
    };
  };

  programs.octofriend = lib.mkIf octofriendCfg.enable {
    instruction = instructionText;
  };

  home.packages = [ ] ++
    (if claudeCodeCfg.enable
    then with pkgs; [
      (pkgs.writeScriptBin "synclaude" ''
        #!${pkgs.runtimeShell}
        # Run Claude Code with Synthetic
        if [ -f "$XDG_CONFIG_HOME/llm-agent/env" ]; then
            . "$XDG_CONFIG_HOME/llm-agent/env"
        fi

        # Run Claude Code with Synthetic
        export SYNTHETIC_MODEL=''${SYNTHETIC_MODEL:-hf:zai-org/GLM-4.7}
        export ANTHROPIC_BASE_URL=https://api.synthetic.new/anthropic
        export ANTHROPIC_AUTH_TOKEN=$SYNTHETIC_API_KEY
        export ANTHROPIC_DEFAULT_OPUS_MODEL=''${ANTHROPIC_DEFAULT_OPUS_MODEL:-$SYNTHETIC_MODEL}
        export ANTHROPIC_DEFAULT_SONNET_MODEL=''${ANTHROPIC_DEFAULT_SONNET_MODEL:-$SYNTHETIC_MODEL}
        export ANTHROPIC_DEFAULT_HAIKU_MODEL=''${ANTHROPIC_DEFAULT_HAIKU_MODEL:-$SYNTHETIC_MODEL}
        export CLAUDE_CODE_SUBAGENT_MODEL=''${CLAUDE_CODE_SUBAGENT_MODEL:-$SYNTHETIC_MODEL}
        export CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1
        export CLAUDE_CONFIG_DIR=$XDG_CONFIG_HOME/claude-synthetic

        exec ${lib.getExe config.programs.claude-code.finalPackage} "$@"
      '')

      (pkgs.writeScriptBin "synthetic-quota" ''
        #!${pkgs.runtimeShell}
        ENV_FILE="$HOME/.config/llm-agent/env"
        if [ -f "$ENV_FILE" ]; then
            . "$ENV_FILE"
        fi

        if [ -z "$SYNTHETIC_API_KEY" ]; then
            echo >&2 "SYNTHETIC_API_KEY is not set"
            exit 1
        fi

        ${lib.getExe pkgs.curl} -s \
            -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
            https://api.synthetic.new/v2/quotas | ${lib.getExe pkgs.jq}
      '')
    ] else [ ]);
}
