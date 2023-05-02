{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
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
        # Apply envvars so that pass, etc. from Home Manager works properly
        if [ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
          . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
        fi

        if printf 't' | ${pkgs.gnupg}/bin/gpg2 -o /dev/null -as - 2>/dev/null; then
          ${pkgs.isync}/bin/mbsync --all
        fi
      '';
    };
  };
}
