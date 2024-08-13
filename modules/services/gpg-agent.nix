{ config, lib, options, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isLinux isDarwin;
  inherit (lib) substring hexStringToBase32 mkForce mkIf;

  gpgDir = config.programs.gpg.homedir;

  gpgconf = dir:
    let
      hash =
        substring 0 24 (hexStringToBase32 (builtins.hashString "sha1" gpgDir));
    in
    if gpgDir == options.programs.gpg.homedir.default then
      "%t/gnupg/${dir}"
    else
      "%t/gnupg/d.${hash}/${dir}";

  gpgInitScript = ''
    GPG_TTY=$(tty); export GPG_TTY
    PINENTRY_USER_DATA=USE_CURSES; export PINENTRY_USER_DATA
  '';

  gpgFishInitScript = ''
    set -gx GPG_TTY (tty)
    set -gx PINENTRY_USER_DATA USE_CURSES
  '';

  pinentryProgramActual =
    if isDarwin then
      "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
    else
      "${pkgs.pinentry.tty}/bin/pinentry-tty";

  pinentryProgram = "${
    pkgs.writeScriptBin "pinentry-wrapper" ''
      #!${pkgs.bash}/bin/bash
      # Wrapper to always force pinentry to use TTY if PINENTRY_USER_DATA=USE_TTY is set.
      # https://superuser.com/questions/1457167/
      pinentry=${pinentryProgramActual}

      case "$PINENTRY_USER_DATA" in
        *USE_TTY* ) pinentry="${pkgs.pinentry.tty}/bin/pinentry-tty";;
        *USE_CURSES* ) pinentry=${pkgs.pinentry.curses}/bin/pinentry-curses;;
      esac

      exec "$pinentry" "$@"
    ''}/bin/pinentry-wrapper";
in
{
  # services.gpg-agent also writes configuration, which is something we
  # want to avoid for compatibility with other platforms.
  #
  # May be required to run:
  #
  #    $ systemctl --user stop gpg-agent.socket
  #    $ systemctl --user start gpg-agent.socket
  #
  # See also: https://github.com/nix-community/home-manager/issues/3567
  systemd.user = mkIf config.machine.isNixOS {
    services.gpg-agent = {
      Unit = {
        Description = "GnuPG cryptographic agent and passphrase cache";
        Documentation = "man:gpg-agent(1)";
        Requires = "gpg-agent.socket";
        After = "gpg-agent.socket";
        # This is a socket-activated service:
        RefuseManualStart = true;
      };

      Service = {
        ExecStart = "${pkgs.gnupg}/bin/gpg-agent --supervised";
        ExecReload = "${pkgs.gnupg}/bin/gpgconf --reload gpg-agent";
        Environment = [ "GNUPGHOME=${gpgDir}" ];
      };
    };

    sockets.gpg-agent = {
      Unit = {
        Description = "GnuPG cryptographic agent and passphrase cache";
        Documentation = "man:gpg-agent(1)";
      };

      Socket = {
        ListenStream = gpgconf "S.gpg-agent";
        FileDescriptorName = "std";
        SocketMode = "0600";
        DirectoryMode = "0700";
      };

      Install = { WantedBy = [ "sockets.target" ]; };
    };
  };

  runit.services = mkIf (isLinux && config.runit.enable) {
    gpg-agent = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        backtick -n -E uid { id -u }
        define xdg-runtime-dir /run/user/''${uid}
        if { test -d ''${xdg-runtime-dir} }

        backtick -n -E agent-socket { ${pkgs.gnupg}/bin/gpgconf --list-dirs agent-socket }
        ${pkgs.s6}/bin/s6-ipcserver-socketbinder -a 0600 ''${agent-socket}
        fdmove 3 0

        export LISTEN_FDS 2
        export LISTEN_FDNAMES std
        getpid LISTEN_PID

        fdmove -c 2 1
        ${pkgs.gnupg}/bin/gpg-agent --supervised
      '';
    };
  };

  home.file = {
    ".gnupg/gpg-agent.conf" = {
      text = ''
        # Pinentry
        allow-emacs-pinentry
        allow-loopback-pinentry
        pinentry-program ${pinentryProgram}

        # TTL
        default-cache-ttl 86400
        max-cache-ttl 604800
      '';
    };
  };

  programs.bash.initExtra = gpgInitScript;
  programs.zsh.initExtra = gpgInitScript;
  programs.fish.interactiveShellInit = gpgFishInitScript;
}
