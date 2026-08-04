{ lib, pkgs, ... }:

let
  tuicrInstructionText = lib.strings.trim ''
    - Use the `tuicr` skill to review code in live sessions.
      - Read user comments with `tuicr review comments`.
      - Add agent comments with `tuicr review add`.
      - Control sessions with `tuicr review *`.
      - Do not use the interactive TUI. It is for the user.
      - Do not set a timeout. A timeout stops the user session.
  '';

  tuicrCommandContext = lib.strings.trim ''
    - `tuicr`: `review list`/`comments` are read-only; `review add` mutates; `tui`/`pr`/`diff` are interactive, for the user.
  '';
in
{
  agents.skillSets.tuicr = "${pkgs.local.skill-tuicr}/skills";

  agents.permissions.default.commands.allow = [ "tuicr" ];

  agents.instructionText = lib.mkAfter tuicrInstructionText;

  agents.commandContext = lib.mkAfter tuicrCommandContext;

  home.packages = [ pkgs.llm-agents.tuicr ];
}
