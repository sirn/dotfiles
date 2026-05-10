{
  config,
  lib,
  pkgs,
  ...
}:

let
  claudeCfg = config.programs.claude-code;
  piCfg = config.programs.pi-coding-agent;

  rtkBin = lib.getExe pkgs.unstable.rtk;
  tomlFormat = pkgs.formats.toml { };

  # Commands to ignore (not proxy commands)
  ignoreCommands = [
    "cc-economics"
    "config"
    "deps"
    "diff"
    "discover"
    "env"
    "err"
    "gain"
    "help"
    "hook"
    "hook-audit"
    "init"
    "json"
    "learn"
    "log"
    "proxy"
    "read"
    "rewrite"
    "smart"
    "summary"
    "test"
    "verify"
  ]
  ++ rtkConfig.hooks.exclude_commands;

  rtkSkillSet =
    pkgs.runCommand "rtk-skill-set"
      {
        nativeBuildInputs = [ pkgs.unstable.rtk ];
        ignore = lib.concatStringsSep " " ignoreCommands;
      }
      ''
        mkdir -p $out/rtk

        cat > $out/rtk/SKILL.md <<'HEADER'
        ---
        name: rtk
        type: reference
        description: Reference for RTK shell output optimization and command rewrite behavior. ALWAYS read when using, configuring, or reasoning about RTK-rewritten shell commands.
        ---

        ## Overview

        RTK (`rtk`) optimizes shell output for agent sessions. It rewrites common commands to compact RTK subcommands and filters noisy output from tools such as test runners, package managers, cloud CLIs, and formatters.

        ## Agent Behavior

        - Shell commands may be automatically rewritten through `rtk rewrite` before execution.
        - Prefer the normal command (`ls`, `pytest`, `cargo test`, etc.) and let the installed hook rewrite it.
        - Do not add extra `head`, `tail`, `jq`, or similar filters just to reduce output when RTK already handles the command.
        - If you must bypass rewrite behavior, prefix the command with `command`, for example: `command ls`.
        - Use `rtk --help` to inspect the currently installed command list.
        - Use `rtk rewrite '<command>'` to preview how a command will be transformed.

        ## Direct RTK Commands

        These are RTK-native utilities rather than transparent command rewrites:

        - `rtk --help` - show available commands
        - `rtk rewrite '<command>'` - preview rewrite behavior
        - `rtk read <file>` - compact file reads
        - `rtk json ...` - compact JSON processing helpers
        - `rtk log ...` - inspect RTK logs
        - `rtk env` - inspect RTK environment/debug state
        - `rtk deps` - inspect RTK dependency information
        - `rtk gain` - estimate output savings

        ## Rewrite Mode

        When rewrite hooks are installed, common commands are transformed automatically. The active installed RTK version reports these rewrite-capable commands:

        HEADER

        ${rtkBin} --help 2>/dev/null | ${pkgs.gawk}/bin/awk -v ignore="$ignore" '
        BEGIN {
          split(ignore, ignarr, " ");
          for (i in ignarr) is_ignored[ignarr[i]] = 1;
        }

        /^Commands:/ { in_commands = 1; next }
        /^Options:/ { in_commands = 0 }

        in_commands && /^  [a-z0-9-]+/ {
          cmd = $1;
          if (is_ignored[cmd]) next;

          desc = substr($0, index($0, $2));
          gsub(/^[ \t]+/, "", desc);
          gsub(/[ \t]+/, " ", desc);

          printf "- `rtk %s <args>` — %s (replaces `%s`)\n", cmd, desc, cmd;
        }
        ' >> $out/rtk/SKILL.md

        cat >> $out/rtk/SKILL.md <<'FOOTER'

        Do not rely on memory for this list. Check `rtk --help` or `rtk rewrite` for the active environment when behavior matters.

        ## Debugging

        1. Run `rtk rewrite '<command>'` to inspect rewrite output.
        2. If rewritten output is inappropriate, retry with `command <original>` to bypass shell/function resolution.
        3. If command permissions are involved, remember some harnesses evaluate the rewritten command rather than the original.
        4. For local configuration, inspect the project's/Home Manager's RTK config before changing global instructions.
        FOOTER
      '';

  rtkInstructionText = lib.strings.trim ''
    - RTK may automatically rewrite shell commands for compact output; to call non-rewrite commands, use `command <command>`.
    - Read the `rtk` skill for rewrite behavior, debugging, and direct RTK commands.
  '';

  rtkRewriteClaudeSh = pkgs.writeShellApplication {
    name = "rtk-rewrite-claude";
    excludeShellChecks = [ "SC2016" ];
    runtimeInputs = [
      pkgs.jaq
      pkgs.unstable.rtk
    ];
    text = builtins.readFile ./rtk-rewrite-claude.sh;
  };

  rtkConfig = {
    hooks = {
      exclude_commands = [
        "cat"
        "curl"
        "find"
        "git"
        "grep"
        "rg"
      ];
    };
  };
in
{
  home.packages = [ pkgs.unstable.rtk ];

  home.file = {
    "Library/Application Support/rtk/config.toml" = lib.mkIf pkgs.stdenv.isDarwin {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    ".pi/agent/extensions/hm-rtk-rewrite/index.ts" = lib.mkIf piCfg.enable {
      text = builtins.replaceStrings [ "\"__RTK_BIN__\"" ] [ "\"${rtkBin}\"" ] (
        builtins.readFile ./rtk-rewrite-pi.ts
      );
    };
  };

  xdg.configFile = {
    "rtk/config.toml" = lib.mkIf pkgs.stdenv.isLinux {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
  };

  programs.claude-code.settings = lib.mkIf claudeCfg.enable {
    hooks = {
      PreToolUse = [
        {
          matcher = "Bash";
          hooks = [
            {
              type = "command";
              command = "${rtkRewriteClaudeSh}/bin/rtk-rewrite-claude";
            }
          ];
        }
      ];
    };
  };

  agents.skillSets.rtk = rtkSkillSet;

  agents.instructionText = lib.mkAfter rtkInstructionText;

  agents.commandContext = lib.mkAfter ''
    `rtk` optimizes shell output for agent sessions by rewriting commands to compact subcommands.
    - `rtk <subcommand> <args>` rewrites common commands (cat, ls, git, etc.) for compact output
    - All `rtk` subcommands are read-only and idempotent — they do not modify files on disk
    - RTK is transparent: `rtk grep` is equivalent to `grep` but with filtered output
    - RTK commands are one-shot transformations; there is no background daemon
  '';

}
