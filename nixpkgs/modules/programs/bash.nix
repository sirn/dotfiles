{ config, pkgs, ... }:

{
  # home-manager only creates .profile when bash is enabled.
  programs.bash = {
    enable = true;
  };
}
