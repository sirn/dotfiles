{ config, lib, pkgs, ... }:

{
  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    COLORTERM = "truecolor";

    # Unless this is set in .profile, Go will loiter go/ in home directory.
    GOPATH = "${config.home.homeDirectory}/Dev/go/gopath:${config.home.homeDirectory}/Dev";
  } // lib.optionalAttrs pkgs.stdenv.isDarwin {
    TERMINFO_DIRS = "${config.home.homeDirectory}/.nix-profile/share/terminfo";
  };

  home.sessionVariablesExtra =
    (lib.optionalString pkgs.stdenv.isDarwin ''
      . "${pkgs.nix}/etc/profile.d/nix-daemon.sh"
    '');
}
