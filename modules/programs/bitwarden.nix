{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;

  agentSocketPath =
    if pkgs.stdenv.isDarwin
    then "${homeDirectory}/.bitwarden-ssh-agent.sock"
    else
      if config.flatpak.enable
      then "${homeDirectory}/.var/app/com.bitwarden.desktop/data/.bitwarden-ssh-agent.sock"
      else "${homeDirectory}/.bitwarden-ssh-agent.sock";
in
{
  home.packages = with pkgs; [
    bitwarden-cli
  ] ++ lib.optional (!config.flatpak.enable) [
    bitwarden-desktop
  ];

  programs.ssh.matchBlocks."*".extraOptions = {
    "IdentityAgent" = lib.mkOverride 250 agentSocketPath;
  };

  xdg.configFile."wezterm/modules/bitwarden.lua" = lib.mkIf config.programs.wezterm.enable {
    text = ''
      return {
        default_ssh_auth_sock = '${agentSocketPath}',
      }
    '';
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
