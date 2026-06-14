{ lib, pkgs, ... }: {
  home.file.".pi/agent/extensions/hm-plan-mode".source = ../vendor/extensions/plan-mode;
}
