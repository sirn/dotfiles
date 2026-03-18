{
  config,
  lib,
  pkgs,
  ...
}:

let
  claudeCfg = config.programs.claude-code;
  piCfg = config.programs.pi-coding-agent;
  opencodeCfg = config.programs.opencode;
  codexCfg = config.programs.codex;
  geminiCfg = config.programs.gemini-cli;

  rtkBin = lib.getExe pkgs.local.rtk;
  tomlFormat = pkgs.formats.toml { };

  # Commands to ignore (not proxy commands)
  ignoreCommands = [
    "err"
    "test"
    "summary"
    "smart"
    "proxy"
    "read"
    "json"
    "deps"
    "env"
    "log"
    "gain"
    "diff"
    "init"
    "discover"
    "learn"
    "verify"
    "hook-audit"
    "rewrite"
    "help"
    "config"
    "cc-economics"
  ]
  ++ rtkConfig.hooks.exclude_commands;

  # Generate RTK instructions dynamically from `rtk --help` output
  rtkInstructionTextFile =
    pkgs.runCommand "rtk-instructions.md"
      {
        nativeBuildInputs = [ pkgs.local.rtk ];
        ignore = lib.concatStringsSep " " ignoreCommands;
      }
      ''
        cat > $out << 'HEADER'
        ## RTK (Shell Output Optimization)

        RTK (`rtk`) is available for compact shell output. Use `rtk --help` to see all available commands. The following commands are automatically rewritten by RTK and should NOT be additionally filtered/parsed with tail, head, jq, etc.:

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

          # Skip ignored commands
          if (is_ignored[cmd]) next;

          # Extract description
          desc = substr($0, index($0, $2));
          gsub(/^[ \t]+/, "", desc);
          gsub(/[ \t]+/, " ", desc);

          printf "- `rtk %s <args>` — %s (replaces `%s`)\n", cmd, desc, cmd;
        }
        ' >> $out
      '';

  rtkInstructionText = builtins.readFile rtkInstructionTextFile;

  rtkRewriteClaudeSh = pkgs.writeShellApplication {
    name = "rtk-rewrite-claude";
    runtimeInputs = [
      pkgs.jq
      pkgs.local.rtk
    ];
    text = builtins.readFile ./rtk-rewrite-claude.sh;
  };

  rtkRewritePiTs = builtins.replaceStrings [ "__RTK_BIN__" ] [ rtkBin ] (
    builtins.readFile ./rtk-rewrite-pi.ts
  );

  rtkRewriteOpencodeTs = builtins.replaceStrings [ "__RTK_BIN__" ] [ rtkBin ] (
    builtins.readFile ./rtk-rewrite-opencode.ts
  );

  rtkConfig = {
    hooks = {
      exclude_commands = [ "curl" ];
    };
  };
in
{
  home.packages = [ pkgs.local.rtk ];

  home.file = {
    "Library/Application Support/rtk/config.toml" = lib.mkIf pkgs.stdenv.isDarwin {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    ".pi/agent/extensions/rtk-rewrite.ts" = lib.mkIf piCfg.enable { text = rtkRewritePiTs; };
  };

  xdg.configFile = {
    "rtk/config.toml" = lib.mkIf pkgs.stdenv.isLinux {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    "opencode/plugins/rtk-rewrite.ts" = lib.mkIf opencodeCfg.enable { text = rtkRewriteOpencodeTs; };
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

  programs.claude-code.memory.text = lib.mkIf claudeCfg.enable (lib.mkAfter rtkInstructionText);
  programs.opencode.rules = lib.mkIf opencodeCfg.enable (lib.mkAfter rtkInstructionText);
  programs.pi-coding-agent.instructionText = lib.mkIf piCfg.enable (lib.mkAfter rtkInstructionText);
  programs.codex.custom-instructions = lib.mkIf codexCfg.enable (lib.mkAfter rtkInstructionText);
  programs.gemini-cli.context.AGENTS = lib.mkIf geminiCfg.enable (lib.mkAfter rtkInstructionText);
}
