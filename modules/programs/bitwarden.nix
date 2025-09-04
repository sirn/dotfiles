{ config, pkgs, ... }:

{
  programs.ssh.matchBlocks."*".extraOptions = {
    "IdentityAgent" =
      if pkgs.stdenv.isDarwin
      then "~/.bitwarden-ssh-agent.sock"
      else
        if config.flatpak.enable
        then "~/.var/app/com.bitwarden.desktop/data/.bitwarden-ssh-agent.sock"
        else "~/.bitwarden-ssh-agent.sock";
  };

  flatpak.applications = {
    "com.bitwarden.desktop" = {
      overrides = {
        environment = {
          ELECTRON_OZONE_PLATFORM_HINT = "wayland";
        };
      };
    };
  };
}
