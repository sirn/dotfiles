{ config, lib, pkgs, ... }:

let
  # Claude Code: Stop hook with transcript fallback
  claudeNotify = pkgs.writeShellApplication {
    name = "claude-code-notify";
    runtimeInputs = [ pkgs.jq pkgs.toastify ];
    text = builtins.readFile ../programs/claude-code/notify.sh;
  };

  # Gemini CLI: AfterAgent hook with prompt_response from stdin
  geminiNotify = pkgs.writeShellApplication {
    name = "gemini-cli-notify";
    runtimeInputs = [ pkgs.jq pkgs.toastify ];
    text = builtins.readFile ../programs/gemini/notify.sh;
  };

  # Codex: notify hook with last-assistant-message from arguments or stdin
  codexNotify = pkgs.writeShellApplication {
    name = "codex-notify";
    runtimeInputs = [ pkgs.jq pkgs.toastify ];
    text = builtins.readFile ../programs/codex/notify.sh;
  };

in
{
  programs.claude-code = lib.mkIf config.programs.claude-code.enable {
    settings = {
      hooks = {
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = lib.getExe claudeNotify;
              }
            ];
          }
        ];
      };
    };
  };

  programs.gemini-cli = lib.mkIf config.programs.gemini-cli.enable {
    settings.hooksConfig.enabled = true;
    settings.hooks.AfterAgent = [
      {
        hooks = [
          {
            type = "command";
            name = "notify-turn-complete";
            command = lib.getExe geminiNotify;
          }
        ];
      }
    ];
  };

  programs.codex = lib.mkIf config.programs.codex.enable {
    settingsOverride.notify = [
      "${pkgs.runtimeShell}"
      (lib.getExe codexNotify)
    ];
  };

  programs.pi-coding-agent = lib.mkIf config.programs.pi-coding-agent.enable {
    extensions = [ "extensions/notify-turn-complete.ts" ];
  };

  xdg.configFile."opencode/plugins/notify-turn-complete.ts" = lib.mkIf config.programs.opencode.enable {
    text = builtins.replaceStrings
      [ "\"__TOASTIFY_BIN__\"" ]
      [ "\"${lib.getExe pkgs.toastify}\"" ]
      (builtins.readFile ../programs/opencode/notify-turn-complete.ts);
  };

  home.file.".pi/agent/extensions/notify-turn-complete.ts" = lib.mkIf config.programs.pi-coding-agent.enable {
    text = builtins.replaceStrings
      [ "\"__TOASTIFY_BIN__\"" ]
      [ "\"${lib.getExe pkgs.toastify}\"" ]
      (builtins.readFile ../programs/pi-coding-agent/notify-turn-complete.ts);
  };
}
