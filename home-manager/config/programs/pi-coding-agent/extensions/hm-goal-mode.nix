{ lib, pkgs, ... }:
let
  goalModePromptsDir = ../vendor/prompts/goal-mode;

  goalModePromptEntries = builtins.listToAttrs (
    builtins.map
      (
        name:
        lib.nameValuePair ".pi/agent/custom/goal-mode/prompts/${name}" {
          text = builtins.readFile (goalModePromptsDir + "/${name}");
        }
      )
      (
        builtins.filter (n: builtins.match ".*\\.md$" n != null) (
          builtins.attrNames (builtins.readDir goalModePromptsDir)
        )
      )
  );
in
{
  home.file = {
    ".pi/agent/extensions/hm-goal-mode".source = ../vendor/extensions/goal-mode;
  }
  // goalModePromptEntries;
}
