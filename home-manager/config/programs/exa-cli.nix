{ lib, pkgs, ... }:

let
  exaInstructionText = lib.strings.trim ''
    - Use the `exa` skill for web search, content extraction, and AI-generated context via the Exa API.
  '';
in
{
  agents.permissions.default.commands.allow = [ "exa" ];

  agents.instructionText = lib.mkAfter exaInstructionText;

  agents.commandContext = lib.mkAfter ''
    `exa` is a CLI for the Exa AI API. `exa search|contents|context` for search/extraction/context, `exa websets` for async websets (use `wait <ID>` to poll). Read-only except `websets create/delete/cancel`.
  '';

  agents.skillSets.exa = {
    path = "${pkgs.local.exa-cli}/skills";
    skills = [ "exa" ];
  };

  home.packages = [ pkgs.local.exa-cli ];
}
