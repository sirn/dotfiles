{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    _1password-cli
  ] ++ lib.optional (!pkgs.stdenv.isDarwin && !config.flatpak.enable) [
    _1password-gui
  ];

  programs.ssh.matchBlocks."*".extraOptions = {
    "IdentityAgent" =
      if pkgs.stdenv.isDarwin
      then "~/Library/Group\\ Containers/2BUA8C4S2C.com.1password/t/agent.sock"
      else "~/.1password/agent.sock";
  };
}
