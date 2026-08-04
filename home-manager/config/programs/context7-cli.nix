{ lib, pkgs, ... }:

let
  context7InstructionText = lib.strings.trim ''
    - Use the `context7` skill for current library documentation.
      - Read the documentation before you implement or write code.
      - Examples include React, the Python standard library, and Rust.
  '';

  context7CommandContext = lib.strings.trim ''
    - `context7` — read-only; read-only API calls, no filesystem modifications, no side effects
  '';

in
{
  agents.permissions.default.commands.allow = [ "context7" ];

  agents.requiredEnvs = [ "CONTEXT7_API_KEY" ];

  agents.instructionText = lib.mkAfter context7InstructionText;

  agents.commandContext = lib.mkAfter context7CommandContext;

  agents.skillSets.context7 = {
    path = "${pkgs.local.context7-cli}/skills";
    skills = [ "context7" ];
  };

  home.packages = [ pkgs.local.context7-cli ];
}
