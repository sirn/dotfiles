{
  lib,
  pkgs,
  config,
  ...
}:

let
  # Absolute paths to allow writes to inside the sandbox.
  extraWriteRules = lib.concatMapStringsSep "\n" (
    path: "(allow file-write* (subpath \"${path}\"))"
  ) config.agents.sandbox.extraWritePaths;

  # Deny all file-read operation classes for a path filter.
  # file-read* alone is insufficient: explicit (allow file-read-data) etc.
  # are more specific and override the wildcard deny.
  denyRead = filter: ''
    (deny file-read* ${filter})
    (deny file-read-data ${filter})
    (deny file-read-metadata ${filter})
    (deny file-read-xattr ${filter})
    (deny file-test-existence ${filter})'';
  secretDenyRules = lib.concatStringsSep "\n    " (
    map denyRead [
      ''(subpath (string-append (param "HOME") "/.ssh"))''
      ''(subpath (string-append (param "HOME") "/.aws"))''
      ''(subpath (string-append (param "HOME") "/.gnupg/private-keys-v1.d"))''
      ''(literal (string-append (param "HOME") "/.gnupg/random_seed"))''
      ''(subpath (string-append (param "HOME") "/.config/sops"))''
      ''(subpath (string-append (param "HOME") "/.config/sops-nix"))''
    ]
  );

  # Seatbelt profile for AI coding agents (pi + claude-code process tree).
  # Default-deny writes; broad reads with secret carve-outs; full network.
  # Imports system.sb for fundamental syscall/mach allowances that Rust and
  # Chrome runtimes need; our (deny default) + specific allows layer on top.
  defaultSeatbeltProfile = ''
    ;; Parameters (supplied via -D): HOME, WORKDIR, XDG_CACHE, XDG_CONFIG, TMPDIR
    (version 1)
    (import "system.sb")
    (deny default)
    (debug deny)

    ;; Network: no egress mediation in the threat model
    (allow network*)

    ;; Process management: tool execution and child processes
    (allow process-exec)
    (allow process-fork)
    (allow signal (target same-sandbox))
    ;; Read-only process inspection (pgrep and other libproc tools).
    ;; /bin/ps and /usr/bin/top are setuid and can't exec inside the sandbox.
    (allow process-info* (target same-sandbox))
    ;; Needed for tty setRawMode (TUI apps like pi/claude)
    (allow file-ioctl)
    (allow pseudo-tty)

    ;; Reads: broad, including /nix/store so skills/CLAUDE.md/AGENTS.md resolve
    ;; Explicit file-read-data/metadata needed because system.sb has specific
    ;; denials that override the file-read* wildcard.
    (allow file-read*)
    (allow file-read-data)
    (allow file-read-metadata)
    (allow file-read-xattr)

    ;; Deny reads of sensitive material (after broad allow: these win)
    ${secretDenyRules}

    ;; Mach IPC: needed by Chrome (crashpad, port rendezvous), launchd, etc.
    (allow mach-lookup)

    ;; Shared memory, I/O Kit, sysctl: needed by Chrome and system APIs
    (allow ipc-posix-shm*)
    (allow ipc-posix-sem)
    (allow ipc-sysv-shm)
    (allow iokit*)
    (allow sysctl*)
    (allow system-info)
    (allow system-socket)
    (allow user-preference*)
    (allow distributed-notification-post)

    ;; Writes: default-denied; allow only working tree + agent state dirs
    (allow file-write* (subpath (param "WORKDIR")))
    (allow file-write* (subpath (string-append (param "HOME") "/.pi")))
    (allow file-write* (subpath (string-append (param "HOME") "/.claude")))
    ${extraWriteRules}
    (allow file-write* (subpath (param "XDG_CACHE")))
    (allow file-write* (subpath (param "XDG_CONFIG")))
    ;; GPG agent sockets (read + write for IPC)
    (allow file-write* (regex (string-append "^" (regex-quote (param "HOME")) "/\\.gnupg/S\\.")))
    (allow file-write* (subpath "/tmp"))
    (allow file-write* (subpath "/private/tmp"))
    (allow file-write* (subpath "/private/var/tmp"))
    (allow file-write* (subpath (param "TMPDIR")))
    (allow file-write* (literal "/dev/null"))
    (allow file-write* (literal "/dev/stdout"))
    (allow file-write* (literal "/dev/stderr"))
    ;; PTY allocation for openpty (tu, agent-browser, interactive tools).
    ;; The slave device requires the com.apple.sandbox.pty extension.
    (allow file-read* file-write* file-ioctl (literal "/dev/ptmx"))
    (allow file-read* file-write*
           (require-all
             (regex #"^/dev/ttys[0-9]+$")
             (extension "com.apple.sandbox.pty")))
    (allow file-ioctl (regex #"^/dev/ttys[0-9]+"))
  '';

  mkWrapper =
    {
      name,
      package,
      preExports ? "",
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
      seatbeltEnabled = config.agents.sandbox.enable && isDarwin;
      profile = pkgs.writeText "${name}-seatbelt.sb" config.agents.sandbox.profile;
      extraEnvExports = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg v}") config.agents.sandbox.extraEnv
      );
      envFileArgs = lib.concatStringsSep " " (map (f: "\"${f}\"") config.agents.sandbox.envFiles);
    in
    pkgs.writeScriptBin name ''
      #!${pkgs.runtimeShell}
      ${lib.strings.trim preExports}
      ${extraEnvExports}
      # Source agent secret env files on the host BEFORE entering the jail,
      # so API keys are in the environment (the jail denies reading these files).
      for _f in ${envFileArgs}; do
        [ -f "$_f" ] && { set -a; . "$_f"; set +a; }
      done
      ${lib.optionalString seatbeltEnabled ''
        if [ "''${_SEATBELT_ACTIVE:-}" != "1" ]; then
          # Resolve TMPDIR to a real path; /var is a symlink to /private/var on macOS,
          # and Seatbelt evaluates paths after symlink resolution.
          _real_tmpdir="$(cd "''${TMPDIR:-/tmp}" 2>/dev/null && pwd -P)"
          _real_tmpdir="''${_real_tmpdir:-/tmp}"
          export _SEATBELT_ACTIVE=1
          exec /usr/bin/sandbox-exec \
            -D "HOME=$HOME" \
            -D "WORKDIR=$PWD" \
            -D "XDG_CACHE=''${XDG_CACHE_HOME:-$HOME/.cache}" \
            -D "XDG_CONFIG=''${XDG_CONFIG_HOME:-$HOME/.config}" \
            -D "TMPDIR=$_real_tmpdir" \
            -f "${profile}" \
            -- "${lib.getExe package}" "$@"
        fi
      ''}
      exec "${lib.getExe package}" "$@"
    '';
in
{
  options.agents.sandbox = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = pkgs.stdenv.hostPlatform.isDarwin;
      description = ''
        Wrap agent processes in a macOS Seatbelt (sandbox-exec) jail.
        No effect on Linux.
      '';
    };

    profile = lib.mkOption {
      type = lib.types.lines;
      default = defaultSeatbeltProfile;
      description = ''
        Seatbelt profile source (Scheme). Consumed by the pi and claude
        wrappers.
      '';
    };

    extraWritePaths = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "/Users/user/.agent-browser"
        "/Users/user/Dev/src"
      ];
      description = ''
        Absolute paths to allow writes to inside the agent sandbox.
        Tool configs contribute their own paths here; the list is empty by
        default. Consumed by the Seatbelt profile.
      '';
    };

    extraEnv = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        AGENT_BROWSER_ARGS = "--no-sandbox";
      };
      description = ''
        Environment variables to export before entering the agent sandbox.
        Tool configs contribute variables here that the sandboxed process tree
        needs (e.g. Chrome's --no-sandbox flag for agent-browser). Exported by
        the pi and claude wrappers.
      '';
    };

    envFiles = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Env files to source before entering the sandbox, so API keys and
        other secrets are in the environment (the jail denies reading these
        files). Sourced in order with shell semantics, so later files can
        reference vars from earlier ones.
      '';
    };

    mkWrapper = lib.mkOption {
      type = lib.types.anything;
      readOnly = true;
      description = ''
        Function that builds a Seatbelt-wrapped script bin for an agent.
        Takes { name, package, preExports ? "" } and returns a writeScriptBin.
      '';
    };
  };

  config = {
    agents.sandbox.mkWrapper = mkWrapper;
  };
}
