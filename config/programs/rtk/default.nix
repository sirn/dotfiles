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

  rtkInstructionText = ''

    ## RTK (Shell Output Optimization)

    RTK (`rtk`) is available for compact shell output. The following commands are useful for analysis and debugging:

    - `rtk help` — show all available RTK commands
    - `rtk curl <url>` — compact HTTP responses
    - `rtk err <cmd>` — run command, show only errors/warnings
    - `rtk log <file>` — show deduplicated log entries with counts
    - `rtk json <file>` — show JSON structure (keys only, no values)
    - `rtk deps` — show compact dependency overview
    - `rtk env` — show environment variables (compact)
    - `rtk summary <cmd>` — smart summary of command output
    - `rtk test <cmd>` — run tests, show failures only
    - `rtk proxy <cmd>` — run command without any RTK filtering
  '';

  rtkBin = lib.getExe pkgs.local.rtk;
  tomlFormat = pkgs.formats.toml { };

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
    ".pi/agent/extensions/rtk-rewrite.ts" = lib.mkIf piCfg.enable {
      text = rtkRewritePiTs;
    };
  };

  xdg.configFile = {
    "rtk/config.toml" = lib.mkIf pkgs.stdenv.isLinux {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    "opencode/plugins/rtk-rewrite.ts" = lib.mkIf opencodeCfg.enable {
      text = rtkRewriteOpencodeTs;
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

  programs.claude-code.memory.text = lib.mkIf claudeCfg.enable (lib.mkAfter rtkInstructionText);
  programs.opencode.rules = lib.mkIf opencodeCfg.enable (lib.mkAfter rtkInstructionText);
  programs.pi-coding-agent.instructionText = lib.mkIf piCfg.enable (lib.mkAfter rtkInstructionText);
  programs.codex.custom-instructions = lib.mkIf codexCfg.enable (lib.mkAfter rtkInstructionText);
  programs.gemini-cli.context.AGENTS = lib.mkIf geminiCfg.enable (lib.mkAfter rtkInstructionText);
}
