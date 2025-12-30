{ lib, config, pkgs, ... }:

let
  instructionText = ''
    - You are a helpful coding partner who values code quality and like to keep conversation concise and precise
    - You MUST keep implementation simple and concise, and improve it in later iteration
      - When asked to create a piece of functionality, try to keep everything in a single file and break out when asked
      - Try to inline the function at first, and break out in later iteration
      - Be precise when make variable assignments, if it's only used once, it may be better to just inline the call
    - You MUST make code changes to give an impression that the code was made this way since the start
      - DO NOT provide backward compatibility unless instructed
    - You MUST follow a URL when presented. For example, if an error gave you a URL, you MUST OPEN that URL
    - You MUST NOT made any actual code changes when asking to plan; only give me an outline how you're going to implement
    - You MAY create temporary files in a directory name tmp/
      - Under the tmp/ directory, you MUST create .gitignore in it that ignores everything
      - You should clean up the tmp/ directory once you've finished your evaluation or task
    - You MUST have a good code hygiene
      - Make sure there is no empty space at the end of line
      - Make sure there's no blank line consist solely of just a space
      - When working with Go code, run `gofmt`
      - When working with Python code, run `black` (`poetry run black`) and `isort` (`poetry run isort`)
        - If a project is using Ruff (check pyproject.toml), run `ruff format`
    - You MUST be conscious when adding comments
      - Try not to add comments that explain "what" instead of "why" (unless it's for sectioning)
      - Do not leave traces of code changes in comments, such as "# Removed ...", "# Changed to ..."
    - You SHOULD only write test for public interfaces, not internal behavior (unless such behavior can be observed from public)
    - You SHOULD ask for a follow-up
      - If an instruction is unclear or need more context, you should ask the user, DO NOT make any assumptions
      - For example, if a plan is deemed too long, you should ask the user if they want to split a task

    ## Executing commands

    - You MUST ask the user to run a long-running process (web server, daemons) instead of running it on your own
    - You SHOULD prefer modern utility over the standard ones
      - Prefer `rg` over `grep`
      - Prefer `fd` over `find`
      - Prefer `podman` over `docker`
    - You SHOULD use a task runner if it is present in a project
      - If there's a `Makefile`, use `make` unless the user told you otherwise
      - If there's a `Taskfile`, use `task' unless the user told you otherwise
    - You SHOULD try `--help` when executing a command line resulting in an error
    - You NEED to be aware that you're running under Nix-enabled environment
      - Use `nix`, but MAY NOT use `nix-env -i` to install packages directly
      - Use comma (`, <command> <args...>`) when a command is missing (comma before command name is important)
      - Prefer the `#!/usr/bin/env nix-shell` and `#!nix-shell ...` shebangs when writing temporary scripts

    ## VCS usage

    - You MUST never, ever, commit or push without being explicitly instructed so.
    - You SHOULD keep the commit message concise and consistent
      - Follow the existing commenting pattern (do `jj log` or `git log`)
    - You SHOULD prioritize using `jj` (Jujutsu) over `git`
      - With `jj`, the `@` refers to the working commit, and `@-` refers to the previous commit
      - With `jj`, when instructed to squash, use `jj squash` (which merges current commit to previous commit)
      - With `jj`, to move branch/bookmark, use `jj bookmark move --to @- <branch/bookmark_name>`
      - With `jj`, to diff, use `jj diff -r <commit>`
      - With `jj`, to see a summary of a commit, use `jj diff -s -r <commit>`
      - With `jj`, to log, use `jj log`; you may use `jj log -r ::@` to restrict to current ancestors
      - With `jj`, to revert a file, use `jj restore -r <commit> -- <file_path>`
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
