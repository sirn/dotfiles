{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;

  agentSocketPath =
    if pkgs.stdenv.isDarwin
    then "${homeDirectory}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
    else "${homeDirectory}/.1password/agent.sock";
in
{
  home.packages = with pkgs; [
    _1password-cli
  ] ++ lib.optional (!pkgs.stdenv.isDarwin && !config.flatpak.enable) [
    _1password-gui
  ];

  programs.ssh.matchBlocks."*".extraOptions = {
    "IdentityAgent" =
      lib.strings.replaceStrings
        [ " " ]
        [ "\\ " ]
        agentSocketPath;
  };

  xdg.configFile."wezterm/hm_ssh.lua" = lib.mkIf config.programs.wezterm.enable {
    text = ''
      return {
        ssh_auth_sock = '${agentSocketPath}',
      }
    '';
  };

  programs.firefox = lib.mkIf config.programs.firefox.enable {
    profiles.main.extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
      onepassword-password-manager
    ];
  };
}
