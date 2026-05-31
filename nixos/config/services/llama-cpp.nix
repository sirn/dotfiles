{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.llama-cpp;
  hasNvidia = builtins.elem "nvidia" config.services.xserver.videoDrivers;
  scrapeHost = if cfg.host == "0.0.0.0" then "localhost" else cfg.host;
in
{
  services.llama-cpp = {
    package = lib.mkIf hasNvidia (
      (pkgs.llama-cpp.override { cudaSupport = true; }).overrideAttrs (old: {
        cmakeFlags = (old.cmakeFlags or [ ]) ++ [
          "-DGGML_CPU_ALL_VARIANTS=ON"
          "-DGGML_BACKEND_DL=ON"
        ];
      })
    );

    extraFlags = [ "--metrics" ];
  };

  services.prometheus.scrapeConfigs = lib.mkIf (cfg.enable && config.services.prometheus.enable) [
    {
      job_name = "llama-cpp";
      static_configs = [ { targets = [ "${scrapeHost}:${toString cfg.port}" ]; } ];
    }
  ];

  systemd.services.llama-cpp = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf hasNvidia {
        path = [ config.hardware.nvidia.package.bin ];
        environment.LD_LIBRARY_PATH = "${config.hardware.nvidia.package}/lib";
        serviceConfig = {
          DeviceAllow = [
            "char-nvidiactl"
            "char-nvidia-caps"
            "char-nvidia-frontend"
            "char-nvidia-uvm"
          ];
          DevicePolicy = lib.mkForce "closed";
          MemoryDenyWriteExecute = lib.mkForce false;
        };
      })
    ]
  );
}
