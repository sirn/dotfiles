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

  # We still don't have launchd gpg-agent.
  gpgSshSupportStr =
    if isDarwin then ''
      ${pkgs.gnupg}/bin/gpg-connect-agent updatestartuptty /bye >/dev/null
    '' else ''
      ${pkgs.gnupg}/bin/gpg-connect-agent --no-autostart updatestartuptty /bye >/dev/null
    '';

  gpgInitScript = ''
    GPG_TTY=$(tty); export GPG_TTY
    ${gpgSshSupportStr}
  '';

  gpgFishInitScript = ''
    set -gx GPG_TTY (tty)
    ${gpgSshSupportStr}
  '';

  pinentryProgram =
    if config.machine.gui.enable && isLinux then
      "${pkgs.pinentry-qt}/bin/pinentry-qt"
    else
      if config.machine.gui.enable && isDarwin then
        "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
      else
        "${pkgs.pinentry}/bin/pinentry";
in
{
  # services.gpg-agent also writes configuration, which is something we
  # want to avoid for compatibility with other platforms.
  #
  # May be required to run:
  #
  #    $ systemctl --user stop gpg-agent{,-ssh}.socket
  #    $ systemctl --user start gpg-agent{,-ssh}.socket
  #
  # See also: https://github.com/nix-community/home-manager/issues/3567
  systemd.user = mkIf config.machine.nixos.enable {
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

    sockets.gpg-agent-ssh = {
      Unit = {
        Description = "GnuPG cryptographic agent (ssh-agent emulation)";
        Documentation =
          "man:gpg-agent(1) man:ssh-add(1) man:ssh-agent(1) man:ssh(1)";
      };

      Socket = {
        ListenStream = gpgconf "S.gpg-agent.ssh";
        FileDescriptorName = "ssh";
        Service = "gpg-agent.service";
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

        ${pkgs.s6}/bin/s6-ipcserver-socketbinder -a 0600 ''${xdg-runtime-dir}/gnupg/S.gpg-agent
        fdmove 3 0
        ${pkgs.s6}/bin/s6-ipcserver-socketbinder -a 0600 ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh
        fdmove 4 0

        export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
        export LISTEN_FDS 2
        export LISTEN_FDNAMES std:ssh
        getpid LISTEN_PID

        fdmove -c 2 1
        if { test -d ''${xdg-runtime-dir} }
        ${pkgs.gnupg}/bin/gpg-agent --supervised
      '';
    };
  };

  # Hack: we have no clean way of starting gpg-agent and setenv at the same time.
  launchd.agents.gpg-agent-userenv = mkIf isDarwin {
    enable = true;
    config = {
      RunAtLoad = true;
      ProgramArguments = [
        "/bin/sh"
        "-l"
        "-c"
        ''
          ${pkgs.gnupg}/bin/gpg-connect-agent /bye;
          launchctl unsetenv SSH_AGENT_PID;
          launchctl setenv SSH_AUTH_SOCK $SSH_AUTH_SOCK
        ''
      ];
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
        default-cache-ttl-ssh 86400
        default-cache-ttl 86400
        max-cache-ttl-ssh 604800
        max-cache-ttl 604800

        # SSH
        enable-ssh-support
        ssh-fingerprint-digest SHA256
      '';
    };
    ".gnupg/sshcontrol" = {
      text = ''
        095FC3D3CC0EC41DDBDD0D33543EF69A4743F949
        51D2F7BE8DE93487063F9089BEBAA4C940660D18
      '';
    };
  };

  home.sessionVariablesExtra = ''
    unset SSH_AGENT_PID
    unset SSH_AUTH_SOCK

    if [ "''${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
      SSH_AUTH_SOCK="''$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)"
      export SSH_AUTH_SOCK
    fi
  '';

  programs.bash.initExtra = gpgInitScript;
  programs.zsh.initExtra = gpgInitScript;
  programs.fish.interactiveShellInit = gpgInitScript;
}
