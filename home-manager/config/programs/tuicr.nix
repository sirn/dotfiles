{ lib, pkgs, ... }:

let
  tuicrInstructionText = lib.strings.trim ''
    - Use the `tuicr` skill to review code through live tuicr sessions.
    - Use `tuicr review comments` to read user comments.
    - Use `tuicr review add` to add agent comments.
    - The tuicr TUI is for the user; drive sessions through `tuicr review *` CLI commands, not interactive TUI commands.
  '';

  tuicrCommandContext = lib.strings.trim ''
    - `tuicr` — `review list`/`comments` are read-only, `review add` mutates a session; `tui`/`pr`/`diff` are interactive, for the user
  '';
in
{
  agents.skillSets.tuicr = "${pkgs.local.skill-tuicr}/skills";

  agents.permissions.default.commands.allow = [ "tuicr" ];

  agents.instructionText = lib.mkAfter tuicrInstructionText;

  agents.commandContext = lib.mkAfter tuicrCommandContext;

  home.packages = [ pkgs.llm-agents.tuicr ];
}
