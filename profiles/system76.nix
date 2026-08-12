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
        ../nixos/config/common.nix
        ../nixos/config/system/network/networkmanager.nix

        # profiles
        ../nixos/config/system/bluetooth.nix
        ../nixos/config/system/desktop.nix
        ../nixos/config/system/game.nix
        ../nixos/config/system/intel-gpu.nix
        ../nixos/config/system/niri.nix
        ../nixos/config/system/pcie-aspm.nix
        ../nixos/config/system/plymouth.nix
        ../nixos/config/system/power-management.nix
        ../nixos/config/system/sway.nix
        ../nixos/config/system/wireless.nix

        # programs
        ../nixos/config/programs/appimage.nix

        # services
        ../nixos/config/services/dnscrypt-proxy.nix
        ../nixos/config/services/flatpak.nix
        ../nixos/config/services/geoclue2.nix
        ../nixos/config/services/greetd.nix
        ../nixos/config/services/node-exporter.nix
        ../nixos/config/services/podman.nix
        ../nixos/config/services/udisks2.nix
        ../nixos/config/services/upower.nix

        # hardware
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

      hardware.uinput.enable = true;

      boot.kernelParams = [
        "i915.enable_guc=3"
        "i915.enable_fbc=1"
        "i915.enable_psr=1"
        "consoleblank=60"
        "pci=noaer"
      ];

      # Fix broken audio with BTD600/BTD700
      services.pipewire.extraConfig.pipewire."99-btd600-btd700-fix" = {
        "context.properties" = {
          "default.clock.min-quantum" = 1024;
          "default.clock.quantum" = 1024;
        };
      };
    };

  home =
    { lib, config, ... }:
    let
      defaultExec = lib.optional config.services.awww.enable "${lib.getExe config.services.awww.package} restore";
    in
    {
      flatpak.enable = true;

      imports = [
        # common
        ../home-manager/config/common.nix
        ../home-manager/config/common-linux.nix

        # profiles
        ../home-manager/config/home/breeze.nix
        ../home-manager/config/home/flatpak.nix
        ../home-manager/config/home/fonts.nix
        ../home-manager/config/home/i18n.nix
        ../home-manager/config/home/laptop.nix
        ../home-manager/config/home/niri/shell.nix
        ../home-manager/config/home/sway/shell.nix
        ../home-manager/config/home/uwsm.nix

        # programs
        ../home-manager/config/programs/bitwarden.nix
        ../home-manager/config/programs/ghostty.nix
        ../home-manager/config/programs/helium.nix
        ../home-manager/config/programs/imagemagick.nix
        ../home-manager/config/programs/mcp.nix
        ../home-manager/config/programs/obsidian.nix
        ../home-manager/config/programs/sublime-text.nix

        # services
        ../home-manager/config/services/coord.nix
        ../home-manager/config/services/handsfree.nix
      ];

      home.colors.themeName = "nord";
      home.colors.variant = "auto";

      services.kanshi = lib.mkIf config.services.kanshi.enable {
        settings = [
          {
            output = {
              alias = "u3425we";
              criteria = "Dell Inc. DELL U3425WE 7WWR3Z3";
              mode = "3440x1440@120Hz";
              position = "0,0";
              adaptiveSync = true;
              scale = 1.0;
            };
          }
          {
            output = {
              alias = "system76";
              criteria = "Chimei Innolux Corporation 0x148A Unknown";
              position = "0,0";
              mode = "1920x1200";
              adaptiveSync = true;
              scale = 1.25;
            };
          }
          {
            profile = {
              name = "dual_system76_aw3225qf";
              outputs = [
                {
                  criteria = "$aw3225qf";
                  status = "enable";
                }
                {
                  criteria = "$system76";
                  status = "disable";
                }
              ];
              exec = defaultExec;
            };
          }
          {
            profile = {
              name = "dual_system76_u3425we";
              outputs = [
                {
                  criteria = "$system76";
                  status = "disable";
                }
                {
                  criteria = "$u3425we";
                  status = "enable";
                  position = "0,0";
                  mode = "3440x1440@120Hz";
                  adaptiveSync = true;
                  scale = 1.0;
                }
              ];
              exec = defaultExec;
            };
          }
          {
            profile = {
              name = "only_system76";
              outputs = [
                {
                  criteria = "$system76";
                  status = "enable";
                }
              ];
              exec = defaultExec;
            };
          }
        ];
      };
    };
}
