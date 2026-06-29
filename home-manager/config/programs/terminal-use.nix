{ lib, pkgs, ... }:

let
  terminalUseInstructionText = lib.strings.trim ''
    - Use `tu` to drive interactive terminal applications and headless terminal sessions: send keystrokes, read the screen, and control the mouse.
    - Load the command reference before the first interaction: `tu usage`.
    - Sessions run under an auto-started background daemon; use `--name` to manage multiple terminal instances.
  '';

  terminalUseCommandContext = lib.strings.trim ''
    - `tu` — drives headless terminal sessions; background daemon auto-starts on first use; `run`/`kill`/`type`/`press`/`paste`/`mouse`/`resize`/`scrollback` mutate the session, `screenshot`/`list`/`status`/`monitor`/`usage` read state
  '';
in
{
  agents.permissions.default.commands.allow = [ "tu" ];

  agents.instructionText = lib.mkAfter terminalUseInstructionText;

  agents.commandContext = lib.mkAfter terminalUseCommandContext;

  home.packages = [ pkgs.local.terminal-use ];
}
