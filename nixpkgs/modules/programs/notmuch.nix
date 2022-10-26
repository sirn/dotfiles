{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  programs.notmuch = {
    enable = !isDarwin;

    new.tags = [ "new" ];
    extraConfig = {
      crypto = {
        gpg_path = "${pkgs.gnupg}/bin/gpg2";
      };
    };

    hooks = {
      preNew = ''
        if printf 't' | ${pkgs.gnupg}/bin/gpg2 -o /dev/null -as - 2>/dev/null; then
          ${mkOutOfStoreSymlink "${dotprivDir}/libexec/notmuch"}/pre_new
          ${pkgs.isync}/bin/mbsync --all
        fi
      '';
      postNew = ''
        ${mkOutOfStoreSymlink "${dotprivDir}/libexec/notmuch"}/post_new
      '';
    };
  };
}
