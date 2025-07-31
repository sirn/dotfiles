{ lib, pkgs, ... }:

let
  wl-clipboard = pkgs.wl-clipboard.overrideDerivation (attrs: {
    buildInputs = (attrs.buildInputs or [ ]) ++ [ pkgs.coreutils ];
    nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
  });

  pbcopy = pkgs.writeScriptBin "pbcopy" ''
    #!${pkgs.bash}/bin/bash
    if test -n "$WAYLAND_DISPLAY"; then
      exec ${wl-clipboard}/bin/wl-copy
    elif test -n "$DISPLAY"; then
      exec ${pkgs.xclip}/bin/xclip -selection clipboard
    else
      echo >&2 "Error: not a desktop?"
      exit 1
    fi
  '';

  pbpaste = pkgs.writeScriptBin "pbpaste" ''
    #!${pkgs.bash}/bin/bash
    if test -n "$WAYLAND_DISPLAY"; then
      exec ${wl-clipboard}/bin/wl-paste
    elif test -n "$DISPLAY"; then
      exec ${pkgs.xclip}/bin/xclip -selection clipboard -o
    else
      echo >&2 "Error: not a desktop?"
      exit 1
    fi
  '';
in
{
  home.packages =
    if pkgs.stdenv.isLinux
    then [ pbcopy pbpaste ]
    else [ ];

  machine.clipboard = lib.mkIf pkgs.stdenv.isLinux {
    copy.command = "${pbcopy}/bin/pbcopy";
    paste.command = "${pbpaste}/bin/pbpaste";
  };
}
