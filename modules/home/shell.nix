{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  home.sessionVariables = {
    LANG = "en_US.UTF-8";

    # Unless this is set in .profile, Go will loiter go/ in home directory.
    GOPATH = "${config.home.homeDirectory}/Dev/go/gopath:${config.home.homeDirectory}/Dev";
  };

  home.sessionPath = [
    "${dotfilesDir}/bin"
    "${config.home.homeDirectory}/.local/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/opt/local/sbin"
    "/opt/local/bin"
  ];

  home.sessionVariablesExtra =
    (lib.optionalString pkgs.stdenv.isDarwin ''
      . "${pkgs.nix}/etc/profile.d/nix-daemon.sh"
    '') +

    # https://github.com/nix-community/home-manager/issues/855
    (lib.optionalString config.machine.isNixOS ''
      ${pkgs.systemd.out}/bin/systemctl --user import-environment PATH
    '');
}
