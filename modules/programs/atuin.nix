{ config, lib, pkgs, ... }:

{
  programs.atuin = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;

    settings = {
      update_check = false;
      show_preview = true;
      filter_mode_shell_up_key_binding = "session";
    };
  };
}
