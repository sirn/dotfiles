{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (lib) mkIf;
in
{
  programs.jujutsu = {
    enable = true;

    settings = {
      user = {
        email = config.programs.git.userEmail;
        name = config.programs.git.userName;
      };

      signing = {
        backend = "gpg";
        key = config.programs.git.signing.key;
        sign-all = true;
      };
    };
  };

  home.file = mkIf (isDarwin && config.programs.jujutsu.settings != { }) {
    "Library/Application Support/jj/config.toml" = {
      source = config.xdg.configFile."jj/config.toml".source;
    };
  };
}
