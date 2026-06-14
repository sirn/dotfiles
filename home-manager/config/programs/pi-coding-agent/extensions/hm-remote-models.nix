{ lib, pkgs, ... }: {
  home.file.".pi/agent/extensions/hm-remote-models".source = ../vendor/extensions/remote-models;
}
