{ lib, config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkIf;
in
{
  xdg = {
    enable = true;

    userDirs = mkIf isLinux {
      enable = true;
      createDirectories = true;
    };

    # Having shared-mime-info on non-NixOS can cause havoc for some apps
    # that don't ignore invalid entries e.g. xdg-desktop-portal-kde
    mime = {
      enable = config.machine.isNixOS;
    };
  };

  home = mkIf isLinux {
    sessionVariablesExtra = ''
      if [ -z "$XDG_RUNTIME_DIR" ]; then
        export XDG_RUNTIME_DIR=/run/user/$(id -u)

        if [ ! -d "$XDG_RUNTIME_DIR" ]; then
          mkdir -p "$XDG_RUNTIME_DIR"
          chmod 0700 "$XDG_RUNTIME_DIR"
        fi
      fi
    '';
  };
}
