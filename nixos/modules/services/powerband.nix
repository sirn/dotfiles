{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.powerband;

  mkOpt =
    type: description:
    lib.mkOption {
      type = lib.types.nullOr type;
      default = null;
      description = description;
    };
in
{
  options.services.powerband = {
    enable = lib.mkEnableOption "powerband";

    eppIdle = mkOpt lib.types.str "EPP value when idle.";
    eppActive = mkOpt lib.types.str "EPP value when active.";

    epbIdle = mkOpt (lib.types.ints.between 0 15) "EPB value when idle (0–15).";
    epbActive = mkOpt (lib.types.ints.between 0 15) "EPB value when active (0–15).";

    governorIdle = mkOpt lib.types.str "Scaling governor when idle.";
    governorActive = mkOpt lib.types.str "Scaling governor when active.";

    upThreshold = mkOpt lib.types.float "Aggregate CPU utilization (0.0–1.0) to go active.";
    downThreshold = mkOpt lib.types.float "Aggregate CPU utilization (0.0–1.0) to go idle.";

    coreUpThreshold = mkOpt lib.types.float "Per-core peak utilization (0.0–1.0) to go active (0 = disabled).";
    coreDownThreshold = mkOpt lib.types.float "Per-core peak utilization (0.0–1.0) to go idle (0 = disabled).";

    rqUpThreshold = mkOpt lib.types.float "Run-queue utilization (0.0–1.0) to go active (0 = disabled).";
    rqDownThreshold = mkOpt lib.types.float "Run-queue utilization (0.0–1.0) to go idle (0 = disabled).";

    pkgPowerUpThreshold = mkOpt lib.types.float "Package power (Watts) to go active (0 = disabled).";
    pkgPowerDownThreshold = mkOpt lib.types.float "Package power (Watts) to go idle (0 = disabled).";

    upDelay = mkOpt lib.types.ints.positive "Sustained seconds above up-threshold before switching to active.";
    downDelay = mkOpt lib.types.ints.positive "Sustained seconds below down-threshold before switching to idle.";

    pollInterval = mkOpt lib.types.ints.positive "Seconds between /proc/stat reads.";

    cpuSysfsPath = mkOpt lib.types.path "Path to CPU sysfs tree.";

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Don't write to sysfs; log what would be done.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.powerband = {
      description = "Load-aware CPU power settings daemon";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = [
          (lib.getExe pkgs.local.powerband)
        ]
        ++ lib.optionals (cfg.eppIdle != null) [
          "--epp-idle"
          cfg.eppIdle
        ]
        ++ lib.optionals (cfg.eppActive != null) [
          "--epp-active"
          cfg.eppActive
        ]
        ++ lib.optionals (cfg.epbIdle != null) [
          "--epb-idle"
          (toString cfg.epbIdle)
        ]
        ++ lib.optionals (cfg.epbActive != null) [
          "--epb-active"
          (toString cfg.epbActive)
        ]
        ++ lib.optionals (cfg.governorIdle != null) [
          "--governor-idle"
          cfg.governorIdle
        ]
        ++ lib.optionals (cfg.governorActive != null) [
          "--governor-active"
          cfg.governorActive
        ]
        ++ lib.optionals (cfg.upThreshold != null) [
          "--up-threshold"
          (lib.strings.floatToString cfg.upThreshold)
        ]
        ++ lib.optionals (cfg.downThreshold != null) [
          "--down-threshold"
          (lib.strings.floatToString cfg.downThreshold)
        ]
        ++ lib.optionals (cfg.coreUpThreshold != null) [
          "--core-up-threshold"
          (lib.strings.floatToString cfg.coreUpThreshold)
        ]
        ++ lib.optionals (cfg.coreDownThreshold != null) [
          "--core-down-threshold"
          (lib.strings.floatToString cfg.coreDownThreshold)
        ]
        ++ lib.optionals (cfg.rqUpThreshold != null) [
          "--rq-up-threshold"
          (lib.strings.floatToString cfg.rqUpThreshold)
        ]
        ++ lib.optionals (cfg.rqDownThreshold != null) [
          "--rq-down-threshold"
          (lib.strings.floatToString cfg.rqDownThreshold)
        ]
        ++ lib.optionals (cfg.pkgPowerUpThreshold != null) [
          "--pkg-power-up-threshold"
          (lib.strings.floatToString cfg.pkgPowerUpThreshold)
        ]
        ++ lib.optionals (cfg.pkgPowerDownThreshold != null) [
          "--pkg-power-down-threshold"
          (lib.strings.floatToString cfg.pkgPowerDownThreshold)
        ]
        ++ lib.optionals (cfg.upDelay != null) [
          "--up-delay"
          (toString cfg.upDelay)
        ]
        ++ lib.optionals (cfg.downDelay != null) [
          "--down-delay"
          (toString cfg.downDelay)
        ]
        ++ lib.optionals (cfg.pollInterval != null) [
          "--poll-interval"
          (toString cfg.pollInterval)
        ]
        ++ lib.optionals (cfg.cpuSysfsPath != null) [
          "--cpu-sysfs-path"
          "${cfg.cpuSysfsPath}"
        ]
        ++ lib.optional cfg.dryRun "--dry-run";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };
  };
}
