{
  config,
  pkgs,
  lib,
  ...
}:

{
  systemd.services = {
    "nvidia-gpu-exporter" = {
      enable = true;
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.prometheus-nvidia-gpu-exporter}/bin/nvidia_gpu_exporter";
        Restart = "always";
        RestartSec = 1;
      };

      path = [
        config.hardware.nvidia.package.bin # nvidia-smi
      ];
    };
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = "nvidia-smi";
      static_configs = [ { targets = [ "localhost:9835" ]; } ];
    }
  ];
}
