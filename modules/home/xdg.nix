{ lib, config, pkgs, ... }:

{
  xdg = {
    enable = true;

    userDirs = lib.mkIf pkgs.stdenv.isLinux {
      enable = true;
      createDirectories = true;
    };

    mime = {
      # Having shared-mime-info on non-NixOS can cause havoc for some apps
      # that don't ignore invalid entries e.g. xdg-desktop-portal-kde
      enable = config.machine.isNixOS;
    };
  };

  home = lib.mkIf pkgs.stdenv.isLinux {
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
