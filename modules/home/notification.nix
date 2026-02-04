{ config, lib, pkgs, ... }:

let
  mkNotify = title: body:
    "${lib.getExe pkgs.toastify} send ${lib.escapeShellArg title} ${lib.escapeShellArg body}";
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
                command = mkNotify "Claude Code" "Claude Code finished their turn";
              }
            ];
          }
        ];
      };
    };
  };

  programs.gemini-cli = lib.mkIf config.programs.gemini-cli.enable {
    settings.hooks.AfterAgent = [
      {
        type = "command";
        name = "notify-turn-complete";
        command = mkNotify "Gemini CLI" "Gemini finished their turn";
      }
    ];
  };

  programs.codex = lib.mkIf config.programs.codex.enable {
    settings.notify =
      let
        notifyScript = pkgs.writeShellScript "codex-notify" ''
          INPUT="$1"
          TYPE=$(echo "$INPUT" | ${lib.getExe pkgs.jq} -r '.type // empty')

          if [ "$TYPE" = "agent-turn-complete" ]; then
            ${mkNotify "Codex" "Codex finished their turn"}
          fi
        '';
      in
      [
        "${pkgs.runtimeShell}"
        notifyScript
        "{}"
      ];
  };

  xdg.configFile."opencode/plugins/notify-turn-complete.ts" = lib.mkIf config.programs.opencode.enable {
    text = ''
      import type { Plugin } from "@opencode-ai/plugin"

      export const NotifyTurnComplete: Plugin = async ({ project, client, $, directory, worktree }) => {
        return {
          event: async ({ event }) => {
            if (event.type === "session.idle") {
              await $`${mkNotify "OpenCode" "OpenCode finished their turn"}`
            }
          },
        }
      }
    '';
  };
}
