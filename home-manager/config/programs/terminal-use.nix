{ lib, pkgs, ... }:

let
  terminalUseInstructionText = lib.strings.trim ''
    - Use `tu` for interactive applications and headless terminal sessions.
      - Use it to send keys, read the screen, and control the mouse.
      - Run `tu usage` before the first interaction.
      - Use `--name` to manage multiple terminal sessions.
      - The background daemon starts automatically.
  '';

  terminalUseCommandContext = lib.strings.trim ''
    - `tu` — drives headless terminal sessions; background daemon auto-starts on first use; `run`/`kill`/`type`/`press`/`paste`/`mouse`/`resize`/`scrollback` mutate the session, `screenshot`/`list`/`status`/`monitor`/`usage` read state
  '';
in
{
  agents.permissions.default.commands.allow = [ "tu" ];

  agents.instructionText = lib.mkAfter terminalUseInstructionText;

  agents.commandContext = lib.mkAfter terminalUseCommandContext;

  home.packages = [ pkgs.llm-agents.terminal-use ];
}
