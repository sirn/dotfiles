{ lib, pkgs, ... }: {
  home.file.".pi/agent/extensions/hm-smart-compact".source = ../vendor/extensions/smart-compact;
}
