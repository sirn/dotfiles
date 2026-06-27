{ lib, pkgs, ... }:

let
  tuicrInstructionText = lib.strings.trim ''
    - Use the `tuicr` skill to review code via live sessions.
    - Read user comments via `tuicr review comments`; add agent comments via `tuicr review add`.
    - Drive sessions via `tuicr review *` CLI; the interactive TUI is user-only.
    - Do not set timeouts on `tuicr` (user-controlled interactive TUI; bounded timeouts kill the session).
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
