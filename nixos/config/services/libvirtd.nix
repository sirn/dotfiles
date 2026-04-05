{ config, pkgs, ... }:

{
  networking.firewall.trustedInterfaces = [ "virbr0" ];

  virtualisation.libvirtd = {
    enable = true;

    # Otherwise guests are suspended/resumed which may
    # lost USB devices passthrough.
    onShutdown = "shutdown";
    onBoot = "ignore";
    parallelShutdown = 10;
    shutdownTimeout = 60;

    extraOptions = [ "--verbose" ];

    qemu = {
      swtpm = {
        enable = true;
      };

      vhostUserPackages = [ pkgs.virtiofsd ];
    };
  };
}
