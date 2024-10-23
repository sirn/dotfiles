{ config, lib, pkgs, ... }:

let
  inherit (config.home) username homeDirectory;

  emacsc = pkgs.writeScriptBin "emacsc" ''
    #!${pkgs.bash}/bin/bash
    # Run Emacs Client on console
    exec ${config.programs.emacs.finalPackage}/bin/emacsclient -nw --alternate-editor= --create-frame "$@"
  '';
in
{
  home.packages = [ emacsc ];

  home.sessionVariables = {
    VISUAL = "${emacsc}/bin/emacsc";
    EDITOR = "${emacsc}/bin/emacsc";
  };
}
