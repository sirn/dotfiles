{
  config,
  pkgs,
  lib,
  ...
}:

let
  textfileCollectorDir = "/run/prometheus-node-exporter-textfile";
in
{
  services.prometheus.exporters.node = {
    extraFlags = [ "--collector.textfile.directory=${textfileCollectorDir}" ];
  };

  systemd.services = {
    "prometheus-node-exporter-ipmitool" = {
      serviceConfig = {
        ExecStart = ''
          ${pkgs.writeScriptBin "prometheus-node-exporter-ipmitool" ''
            #!${pkgs.bash}/bin/bash
            mkdir -p ${textfileCollectorDir}
            ${pkgs.ipmitool}/bin/ipmitool sensor | ${pkgs.local.node-textfile-collector-scripts}/libexec/node-exporter-textfile-collector-scripts/ipmitool | ${pkgs.moreutils}/bin/sponge ${textfileCollectorDir}/ipmitool_sensor.prom
          ''}/bin/prometheus-node-exporter-ipmitool
        '';
      };
    };
  };

  systemd.timers = {
    "prometheus-node-exporter-ipmitool" = {
      enable = true;
      description = "Generates node-exporter textfile metrics using ipmitool";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnBootSec = 60;
        OnUnitActiveSec = 60;
        AccuracySec = "1us";
      };
    };
  };
}
