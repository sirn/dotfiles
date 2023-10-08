{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;
  inherit (config.home) homeDirectory;

  dotfilesDir = "${homeDirectory}/.dotfiles";
  dotprivDir = "${homeDirectory}/.dotfiles";
in
{
  home.sessionVariables = {
    EDITOR = "emacs";
    VISUAL = "emacs";

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

  home.shellAliases = {
    ll = "ls -al";
    sshi = "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null";
  };

  home.sessionVariablesExtra = lib.optionalString isDarwin ''
    . "${pkgs.nix}/etc/profile.d/nix-daemon.sh"
  '';
}
