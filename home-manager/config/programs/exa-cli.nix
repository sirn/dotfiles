{ lib, pkgs, ... }:

let
  exaInstructionText = lib.strings.trim ''
    - Use the `exa` skill for web search, content extraction, and AI-generated context via the Exa API.
  '';

  exaCommandContext = lib.strings.trim ''
    - `exa` — read-only (search, contents, context); `websets create/delete/cancel` make network writes
  '';

in
{
  agents.permissions.default.commands.allow = [ "exa" ];

  agents.requiredEnvs = [ "EXA_API_KEY" ];

  agents.instructionText = lib.mkAfter exaInstructionText;

  agents.commandContext = lib.mkAfter exaCommandContext;

  agents.skillSets.exa = {
    path = "${pkgs.local.exa-cli}/skills";
    skills = [ "exa" ];
  };

  home.packages = [ pkgs.local.exa-cli ];
}
