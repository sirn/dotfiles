{ lib, ... }:

let
  readMdFiles = import ./lib/read-md-files.nix { inherit lib; };
  planModePromptsDir = ../vendor/prompts/plan-mode;
in
{
  home.file = {
    ".pi/agent/extensions/hm-plan-mode".source = ../vendor/extensions/plan-mode;
  }
  // readMdFiles {
    dir = planModePromptsDir;
    prefix = ".pi/agent/custom/plan-mode/prompts";
  };
}
