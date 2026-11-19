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

    hashicorp-terraform = {
      path = "${pkgs.local.skill-hashicorp}/skills/terraform";
      skills = [
        "refactor-module"
        "terraform-policy"
        "terraform-search-import"
        "terraform-stacks"
        "terraform-style-guide"
        "terraform-test"
      ];
    };

    simple-english = "${pkgs.local.skill-simple-english}/skills";

    superpowers = {
      path = "${pkgs.local.skill-superpowers}/skills";
      skills = [
        "systematic-debugging"
        "test-driven-development"
      ];
    };
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
