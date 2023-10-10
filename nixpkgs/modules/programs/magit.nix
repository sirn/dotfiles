{ config, lib, pkgs, ... }:

let
  inherit (config.home) username homeDirectory;

  dotfilesDir = "${homeDirectory}/.dotfiles";

  magit = pkgs.writeScriptBin "magit" ''
    #!${pkgs.bash}/bin/bash
    # Runs Magit in a standalone mode
    if [ "$(git rev-parse --is-inside-work-tree)" = "true" ]; then
        exec emacs -q --no-splash -nw \
            -l "${config.programs.emacs.finalPackage.deps}/share/emacs/site-lisp/site-start.el" \
            -l "${dotfilesDir}/etc/emacs/magit.el"
        fi
  '';
in
{
  home.packages = [ magit ];
}
