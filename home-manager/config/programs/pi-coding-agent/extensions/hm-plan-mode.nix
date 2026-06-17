{ lib, pkgs, ... }:
let
  planModePromptsDir = ../vendor/prompts/plan-mode;

  planModePromptEntries = builtins.listToAttrs (
    builtins.map
      (
        name:
        lib.nameValuePair ".pi/agent/custom/plan-mode/prompts/${name}" {
          text = builtins.readFile (planModePromptsDir + "/${name}");
        }
      )
      (
        builtins.filter (n: builtins.match ".*\\.md$" n != null) (
          builtins.attrNames (builtins.readDir planModePromptsDir)
        )
      )
  );
in
{
  home.file = {
    ".pi/agent/extensions/hm-plan-mode".source = ../vendor/extensions/plan-mode;
  }
  // planModePromptEntries;
}
