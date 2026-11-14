{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.programs.obsidian-headless;

  syncCommand = path: [
    (lib.getExe cfg.package)
    "sync"
    "--continuous"
    "--path"
    path
  ];

  systemdServices = lib.mapAttrs' (
    name: path:
    lib.nameValuePair "obsidian-headless-sync-${name}" {
      Install.WantedBy = [ "default.target" ];

      Unit = {
        Description = "Obsidian Sync for vault \"${name}\"";
        Documentation = "https://github.com/obsidianmd/obsidian-headless";
      };

      Service = {
        ExecStart = lib.escapeShellArgs (syncCommand path);
        Slice = lib.mkDefault "app.slice";
        Restart = "on-failure";
        RestartSec = 10;
      };
    }
  ) cfg.sync.vaults;

  launchdAgents = lib.mapAttrs' (
    name: path:
    lib.nameValuePair "obsidian-headless-sync-${name}" {
      enable = true;
      config = {
        RunAtLoad = true;
        KeepAlive = true;
        ProgramArguments = syncCommand path;
      };
    }
  ) cfg.sync.vaults;
in
{
  options.programs.obsidian-headless = {
    enable = mkEnableOption "obsidian-headless, a headless CLI client for Obsidian Sync and Obsidian Publish";

    package = mkOption {
      type = types.package;
      default = pkgs.unstable.obsidian-headless;
      defaultText = "pkgs.unstable.obsidian-headless";
      description = "The obsidian-headless package to use.";
    };

    sync = {
      enable = mkEnableOption "continuous sync, which keeps each configured vault synced by watching for changes";

      vaults = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = {
          notes = "/home/alice/vaults/notes";
        };
        description = ''
          Vaults to sync while continuous sync is enabled, as an attribute set
          mapping a name to the local vault directory. Each vault runs
          `ob sync --continuous` in its own user service.

          Vaults must be linked with `ob sync-setup` and the account must be
          logged in with `ob login` beforehand.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.sync.enable || cfg.sync.vaults != { };
        message = "programs.obsidian-headless.sync.enable requires at least one vault in programs.obsidian-headless.sync.vaults.";
      }
    ];

    home.packages = [ cfg.package ];

    systemd.user.services = lib.mkIf pkgs.stdenv.isLinux systemdServices;

    launchd.agents = lib.mkIf pkgs.stdenv.isDarwin launchdAgents;
  };
}
