{ config, pkgs, lib, ... }:

let
  cfg = config.services.gpg-agent;

  gpgcfg = config.programs.gpg;

  swaycfg = config.wayland.windowManager.sway;
  niricfg = config.programs.niri;

  hasGui = swaycfg.enable || niricfg.enable;
in
{
  services.gpg-agent = {
    enable = gpgcfg.enable;
    enableSshSupport = true;

    pinentry = {
      package = lib.mkDefault (
        if pkgs.stdenv.isDarwin then
          pkgs.pinentry_mac
        else if hasGui then
          pkgs.pinentry-qt
        else
          pkgs.pinentry-curses
      );
    };

    defaultCacheTtl = 21600;
    defaultCacheTtlSsh = 21600;
    maxCacheTtl = 43200;
    maxCacheTtlSsh = 43200;

    # gpg --list-keys --with-keygrip
    sshKeys = [
      "DB2F6C327247DA184010A3181710D1C56B427410"
    ];

    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  # Home Manager default launchd agent uses --supervised which depends on systemd sockets
  # holding a socket and passing fds via `LISTEN_FDS`. This obviously doesn't work on Darwin
  # since `Sockets` requires explicit `launch_activate_socket` call in the code.
  launchd.agents =
    let
      sockets = {
        std = "${gpgcfg.homedir}/S.gpg-agent";
        browser = "${gpgcfg.homedir}/S.gpg-agent.browser";
        extra = "${gpgcfg.homedir}/S.gpg-agent.extra";
        ssh = "${gpgcfg.homedir}/S.gpg-agent.ssh";
      };

      socket_fdnames = lib.concatStringsSep ":" (lib.attrNames sockets);

      socket_args = lib.flatten (lib.mapAttrsToList
        (key: val: [
          "-s"
          "unix::${val}"
        ])
        sockets);
    in
    lib.mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
      gpg-agent = lib.mkForce {
        enable = true;
        config = {
          RunAtLoad = true;
          KeepAlive = true;

          # We're using systemfd to pass socket fd so that --supervise works.
          # This is in combination with LISTEN_FDNAMES set in EnvironmentVariables.
          # Why must launching gpg-agent be this painful? I don't know.
          ProgramArguments = [
            "${pkgs.systemfd}/bin/systemfd"
            "--no-pid"
          ] ++ socket_args ++ [
            "--"
            "${gpgcfg.package}/bin/gpg-agent"
            "--supervise"
          ] ++ lib.optional cfg.verbose "--verbose";

          EnvironmentVariables = {
            GNUPGHOME = gpgcfg.homedir;
            LISTEN_FDNAMES = socket_fdnames;
          };
        };
      };
    };

  programs.ssh.matchBlocks."*".extraOptions = lib.mkIf cfg.enable {
    IdentityAgent = lib.mkOverride 500 "$\{XDG_RUNTIME_DIR\}/gnupg/S.gpg-agent.ssh";
  };

  systemd.user.services.gpg-agent.Service = lib.mkIf cfg.enable {
    Slice = lib.mkDefault "app.slice";
  };
}
