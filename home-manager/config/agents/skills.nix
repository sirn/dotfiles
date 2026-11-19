{ lib, pkgs, ... }:

let
  simpleEnglishPrompt = lib.strings.trim (
    builtins.readFile "${pkgs.local.skill-simple-english}/prompt.md"
  );
in
{
  agents.skillSets = {
    apis = ../../../var/agents/skills/apis;
    coding = ../../../var/agents/skills/coding;
    lifecycle = ../../../var/agents/skills/lifecycle;
    references = ../../../var/agents/skills/references;
    simple-english = "${pkgs.local.skill-simple-english}/skills";
  };

  agents.instructionText = lib.mkOrder 1200 (
    "\n"
    + lib.strings.trim ''
      ## Simple English

      Use the `simple-english` skill for strict ASD-STE100 compliance checks or full rule references.

      ${simpleEnglishPrompt}
    ''
  );
}
