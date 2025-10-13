{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    bitwarden-cli
  ] ++ lib.optional (!config.flatpak.enable) [
    bitwarden-desktop
  ];

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

  programs.firefox = lib.mkIf config.programs.firefox.enable {
    profiles.main.extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
      bitwarden
    ];
  };
}
