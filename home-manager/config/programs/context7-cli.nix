{ lib, pkgs, ... }:

let
  context7InstructionText = lib.strings.trim ''
    - Use the `context7` skill for retrieving up-to-date library documentation (React, Python stdlib, Rust, etc.) BEFORE implementing or writing code.
  '';

  context7CommandContext = lib.strings.trim ''
    - `context7` — read-only; read-only API calls, no filesystem modifications, no side effects
  '';

in
{
  agents.permissions.default.commands.allow = [ "context7" ];

  agents.instructionText = lib.mkAfter context7InstructionText;

  agents.commandContext = lib.mkAfter context7CommandContext;

  agents.skillSets.context7 = {
    path = "${pkgs.local.context7-cli}/skills";
    skills = [ "context7" ];
  };

  home.packages = [ pkgs.local.context7-cli ];
}
