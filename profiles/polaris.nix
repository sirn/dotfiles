{
  nixos = { nixos-hardware, ... }: {
    imports = [
      ../nixos/config/common.nix
      ../nixos/config/common-zfs.nix
      ../nixos/config/system/network/networkmanager.nix

      # machine
      nixos-hardware.nixosModules.lenovo-thinkpad-x1-13th-gen

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
      ../nixos/config/programs/1password.nix
      ../nixos/config/programs/appimage.nix
      ../nixos/config/programs/ddcutil.nix

      # services
      ../nixos/config/services/flatpak.nix
      ../nixos/config/services/geoclue2.nix
      ../nixos/config/services/fwupd.nix
      ../nixos/config/services/greetd.nix
      ../nixos/config/services/node-exporter.nix
      ../nixos/config/services/podman.nix
      ../nixos/config/services/prometheus-agent.nix
      ../nixos/config/services/sanoid.nix
      ../nixos/config/services/thermald.nix
      ../nixos/config/services/tlp.nix
      ../nixos/config/services/upower.nix
    ];

    networking.hostName = "polaris";

    # Backups
    services.sanoid = {
      datasets = {
        "zroot/DATA" = {
          useTemplate = [ "daily" ];
          processChildrenOnly = true;
          recursive = true;
        };
        "zroot/ROOT" = {
          useTemplate = [ "daily" ];
          processChildrenOnly = true;
          recursive = true;
        };
      };
    };

    # Set default backlight
    services.udev.extraRules = ''
      SUBSYSTEM=="backlight", ACTION=="add", KERNEL=="intel_backlight", ATTR{brightness}="100"

      # Prevent the internal I2C touchpad from waking the laptop repeatedly while suspended.
      SUBSYSTEM=="i2c", ACTION=="add|change", KERNEL=="i2c-SNSL0028:00", ATTR{power/wakeup}="disabled"
    '';

    hardware.uinput.enable = true;

    # Fix broken audio on Lunar Lake.
    services.pipewire.extraConfig.pipewire."99-lunar-lake-fix" = {
      "context.properties" = {
        "default.clock.min-quantum" = 1024;
        "default.clock.quantum" = 1024;
      };
    };

    # Attempt at fixing ZFS crash on `arc_evict` at `__pgalloc_tag_sub`
    # which seems to be enabled as part of `mem_alloc_profiling_enabled`
    # aka vm.mem_profiling.
    #
    # Not sure if it's a bug in ZFS or the kernel. Let's disable this
    # for the time being and see if it will fix the issue. So far, this
    # crash is observed on ThinkPad X1 Carbon Gen 13 (Intel Core 258V).
    #
    # TODO: reevaluate after 6.17 or if the crash happens again
    boot.kernel.sysctl = {
      "vm.mem_profiling" = "0";
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
        ../home-manager/config/programs/1password.nix
        ../home-manager/config/programs/brightnessctl.nix
        ../home-manager/config/programs/ffmpeg.nix
        ../home-manager/config/programs/ghostty.nix
        ../home-manager/config/programs/helium.nix
        ../home-manager/config/programs/imagemagick.nix
        ../home-manager/config/programs/intellij.nix
        ../home-manager/config/programs/mpv.nix
        ../home-manager/config/programs/sublime-text.nix
        ../home-manager/config/programs/virt-manager.nix
        ../home-manager/config/services/coord.nix
        ../home-manager/config/programs/yt-dlp.nix
      ];

      services.kanshi = lib.mkIf config.services.kanshi.enable {
        settings = [
          {
            output = {
              alias = "internal";
              criteria = "Samsung Display Corp. ATNA40HQ02-0  Unknown";
              mode = "2880x1800@120Hz";
              scale = 1.75;
            };
          }
          {
            profile = {
              name = "only_internal";
              outputs = [ { criteria = "$internal"; } ];
              exec = defaultExec;
            };
          }
          {
            profile = {
              name = "dual_aw3225qf_internal";
              outputs = [
                { criteria = "$aw3225qf"; }
                {
                  criteria = "$internal";
                  status = "disable";
                }
              ];
              exec = defaultExec;
            };
          }
        ];
      };
    };
}
