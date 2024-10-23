{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (config.home) homeDirectory;

  dotfilesDir = "${homeDirectory}/.dotfiles";
in
{
  home.sessionVariables = {
    LANG = "en_US.UTF-8";

    # Unless this is set in .profile, Go will loiter go/ in home directory.
    GOPATH = "${homeDirectory}/Dev/go/gopath:${homeDirectory}/Dev";
  };

  home.sessionPath = [
    "${dotfilesDir}/bin"
    "${homeDirectory}/.local/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/opt/local/sbin"
    "/opt/local/bin"
  ];

  home.sessionVariablesExtra =
    (lib.optionalString isDarwin ''
      . "${pkgs.nix}/etc/profile.d/nix-daemon.sh"
    '') +

    # https://github.com/nix-community/home-manager/issues/855
    (lib.optionalString config.machine.isNixOS ''
      ${pkgs.systemd.out}/bin/systemctl --user import-environment PATH
    '');
}
