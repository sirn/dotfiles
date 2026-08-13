{
  nixos = { nixos-hardware, pkgs, ... }: {
    imports = [
      ../nixos/config/roles/laptop.nix
      ../nixos/config/common-zfs.nix

      # Machine
      nixos-hardware.nixosModules.lenovo-thinkpad-x1-13th-gen

      # Programs
      ../nixos/config/programs/1password.nix
      ../nixos/config/programs/ddcutil.nix

      # Services
      ../nixos/config/services/fwupd.nix
      ../nixos/config/services/prometheus-agent.nix
      ../nixos/config/services/sanoid.nix
      ../nixos/config/services/thermald.nix
      ../nixos/config/services/tlp.nix
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

    # Rebind the I2C HID touchpad driver on resume to work around a Sensel
    # SNSL0028:00 trackpad bug where scrolling stops working after s2idle
    # suspend. The hid-multitouch driver doesn't properly re-initialize
    # the device on resume.
    #
    # Skip the rebind if hid-multitouch is already bound. After rebinding,
    # poll with 1-second backoff (up to 5 times) until hid-multitouch claims
    # the device — hid-generic binds first before hid-multitouch takes over.
    powerManagement.resumeCommands = ''
      ${pkgs.writeShellScript "polaris-rebind-touchpad" ''
        set -euo pipefail

        HID_DRIVER=/sys/bus/i2c/drivers/i2c_hid_acpi
        DEVICE=i2c-SNSL0028:00
        HID_ID=0018:2C2F:0028
        MAX_RETRIES=5

        is_multitouch() {
          local driver
          driver=$(readlink "$HID_DRIVER/$DEVICE"/"$HID_ID".*/driver 2>/dev/null || true)
          [[ "$driver" == *hid-multitouch* ]]
        }

        wait_for_multitouch() {
          local retries=$1
          if is_multitouch; then
            return 0
          fi
          if [ "$retries" -ge "$MAX_RETRIES" ]; then
            echo "polaris-rebind-touchpad: hid-multitouch did not bind after $MAX_RETRIES retries" >&2
            return 1
          fi
          sleep 1
          wait_for_multitouch $((retries + 1))
        }

        # Skip if hid-multitouch is already correctly bound.
        if is_multitouch; then
          exit 0
        fi

        # Rebind to force a full re-probe.
        if [ -e "$HID_DRIVER/$DEVICE" ]; then
          echo "$DEVICE" > "$HID_DRIVER/unbind" || true
          sleep 1
          echo "$DEVICE" > "$HID_DRIVER/bind" || true
        fi

        wait_for_multitouch 0
      ''}
    '';

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
      imports = [
        ../home-manager/config/roles/laptop.nix

        # Programs
        ../home-manager/config/programs/1password.nix
        ../home-manager/config/programs/brightnessctl.nix
        ../home-manager/config/programs/ffmpeg.nix
        ../home-manager/config/programs/intellij.nix
        ../home-manager/config/programs/mpv.nix
        ../home-manager/config/programs/virt-manager.nix
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
