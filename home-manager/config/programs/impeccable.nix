{ lib, pkgs, ... }:

let
  impeccableInstructionText = lib.strings.trim ''
    - Use the `impeccable` skill (and its preinstalled `impeccable` CLI) for frontend UI design, critique, audit, and polish.
  '';

  impeccableCommandContext = lib.strings.trim ''
    - `impeccable` — frontend design engine CLI (`context`, `detect`, `shot`, `font-fetch`, `engine-probe`); mostly read-only inspection and rendering checks, `font-fetch` downloads font assets
  '';
in
{
  agents.skillSets.impeccable = "${pkgs.local.impeccable}/skills";

  agents.permissions.default.commands.allow = [ "impeccable" ];

  agents.instructionText = lib.mkAfter impeccableInstructionText;

  agents.commandContext = lib.mkAfter impeccableCommandContext;

  home.packages = [ pkgs.local.impeccable ];
}
