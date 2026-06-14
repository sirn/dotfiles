{ lib, pkgs, ... }: {
  home.file.".pi/agent/extensions/hm-notify-turn-complete".source =
    ../vendor/extensions/notify-turn-complete;
}
