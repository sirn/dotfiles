{ lib, ... }:

let
  readMdFiles = import ./lib/read-md-files.nix { inherit lib; };
  goalModePromptsDir = ../vendor/prompts/goal-mode;
in
{
  home.file = {
    ".pi/agent/extensions/hm-goal-mode".source = ../vendor/extensions/goal-mode;
  }
  // readMdFiles {
    dir = goalModePromptsDir;
    prefix = ".pi/agent/custom/goal-mode/prompts";
  };
}
