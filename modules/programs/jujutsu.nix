{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (lib) mkIf;
in
{
  programs.jujutsu = {
    enable = true;

    package = pkgs.unstable.jujutsu;

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

      ui = {
        default-command = "log";
        diff-editor = ":builtin";
        editor = config.home.sessionVariables.EDITOR;
        pager = "${pkgs.less}/bin/less";
      };
    };
  };

  home.file = mkIf (isDarwin && config.programs.jujutsu.settings != { }) {
    "Library/Application Support/jj/config.toml" = {
      source = config.xdg.configFile."jj/config.toml".source;
    };
  };
}
