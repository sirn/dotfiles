{
  nixos =
    {
      lib,
      pkgs,
      nixos-hardware,
      ...
    }:
    let
      systemdRun = lib.getExe' pkgs.systemd "systemd-run";
      system76Power = lib.getExe pkgs.system76-power;
    in
    {
      imports = [
        ../nixos/config/roles/laptop.nix

        # Services
        ../nixos/config/services/dnscrypt-proxy.nix
        ../nixos/config/services/udisks2.nix

        # Hardware
        "${nixos-hardware}/common/cpu/intel"
        "${nixos-hardware}/common/cpu/intel/meteor-lake"
        "${nixos-hardware}/common/gpu/intel/meteor-lake"
      ];

      networking.hostName = "system76";

      # Bootloader
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.efi.efiSysMountPoint = "/boot/efi";

      # Power Management
      hardware.system76.enableAll = true;
      services.thermald.enable = true;
      powerManagement.powertop.enable = true;

      # Limit battery charge to 75-80% for battery longevity
      systemd.services.system76-battery-limit = {
        description = "Set System76 battery charge threshold to 80%";
        after = [ "system76-power.service" ];
        wants = [ "system76-power.service" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.system76-power}/bin/system76-power charge-thresholds 75 80";
        };
        wantedBy = [ "multi-user.target" ];
      };

      # Automatic power profile switching based on AC state
      services.udev.extraRules = ''
        # AC plugged in - switch to balanced profile
        SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${system76Power} profile balanced"
        SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${system76Power} profile balanced"

        # On battery - switch to battery profile
        SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${system76Power} profile battery"
        SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${system76Power} profile battery"
      '';

      boot.kernelParams = [
        "consoleblank=60"
        "pci=noaer"
      ];
    };

  home =
    { lib, config, ... }:
    let
      defaultExec = lib.optional config.services.awww.enable "${lib.getExe config.services.awww.package} restore";
    in
    {
      imports = [
        ../home-manager/config/roles/laptop.nix

        # Programs
        ../home-manager/config/programs/bitwarden.nix
        ../home-manager/config/programs/mcp.nix
      ];

      home.colors.themeName = "nord";
      home.colors.variant = "auto";

      services.kanshi = lib.mkIf config.services.kanshi.enable {
        settings = [
          {
            output = {
              alias = "internal";
              criteria = "Chimei Innolux Corporation 0x148A Unknown";
              position = "0,0";
              mode = "1920x1200";
              adaptiveSync = true;
              scale = 1.25;
            };
          }
          {
            profile = {
              name = "only_internal";
              outputs = [
                { criteria = "$internal"; }
              ];
              exec = defaultExec;
            };
          }
          {
            profile = {
              name = "dual_aw3225qf_internal";
              outputs = [
                {
                  criteria = "$aw3225qf";
                  position = "0,0";
                }
                {
                  criteria = "$internal";
                  position = "1056,1728"; # below-center of aw3225qf
                }
              ];
              exec = defaultExec;
            };
          }
          {
            profile = {
              name = "dual_u3425we_internal";
              outputs = [
                {
                  criteria = "$internal";
                  position = "0,0";
                }
                {
                  criteria = "$u3425we";
                  position = "1536,960"; # right of internal
                }
              ];
              exec = defaultExec;
            };
          }
        ];
      };
    };
}
