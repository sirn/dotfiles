{
  config,
  lib,
  pkgs,
  ...
}:

let
  waitForPamKWallet = pkgs.writeShellScript "wait-for-pam-kwallet" ''
    for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
      owner="$(${pkgs.systemd}/bin/busctl --user call \
        org.freedesktop.DBus \
        /org/freedesktop/DBus \
        org.freedesktop.DBus \
        GetNameOwner \
        s org.kde.ksecretd 2>/dev/null \
        | ${pkgs.gnused}/bin/sed -n 's/^s "\(.*\)"$/\1/p')"

      if [ -n "$owner" ]; then
        pid="$(${pkgs.systemd}/bin/busctl --user call \
          org.freedesktop.DBus \
          /org/freedesktop/DBus \
          org.freedesktop.DBus \
          GetConnectionUnixProcessID \
          s "$owner" 2>/dev/null \
          | ${pkgs.gnused}/bin/sed -n 's/^u //p')"

        if [ -n "$pid" ] \
          && ${pkgs.coreutils}/bin/tr '\0' ' ' < "/proc/$pid/cmdline" \
          | ${pkgs.gnugrep}/bin/grep -q -- '--pam-login'
        then
          exit 0
        fi
      fi

      ${pkgs.coreutils}/bin/sleep 0.1
    done

    exit 1
  '';
in
{
  systemd.user.services.kwallet-pam =
    lib.mkIf (pkgs.stdenv.isLinux && !config.targets.genericLinux.enable)
      {
        Unit = {
          Description = "KWallet PAM Init";
          PartOf = [ config.wayland.systemd.target ];
          Before = [ "xdg-desktop-portal.service" ];

          # Avoid re-running kwallet-pam.
          X-RestartIfChanged = false;
        };

        Service = {
          Type = "oneshot";
          ExecStart = with pkgs; "-${kdePackages.kwallet-pam}/libexec/pam_kwallet_init";
          ExecStartPost = waitForPamKWallet;
          Slice = lib.mkDefault "app.slice";
          RemainAfterExit = "yes";
          KillMode = "control-group";
        };

        Install = {
          WantedBy = [ config.wayland.systemd.target ];
        };
      };

  xdg.configFile."systemd/user/xdg-desktop-portal.service.d/10-kwallet-pam.conf" =
    lib.mkIf (pkgs.stdenv.isLinux && !config.targets.genericLinux.enable)
      {
        text = ''
          [Unit]
          Requires=kwallet-pam.service
          After=kwallet-pam.service
        '';
      };
}
