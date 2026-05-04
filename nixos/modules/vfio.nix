{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.vfio;

  tomlFormat = pkgs.formats.toml { };

  udevForwarderDomainMatchOpts = lib.types.submodule {
    options = {
      devpath = lib.mkOption {
        type = lib.types.str;
        description = ''
          The device path to match, e.g. /devices/pci0000:00/0000:00:14.0/usb1/1-9
        '';
      };
    };
  };

  udevForwarderDomainOpts = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = ''
          The name of the VM.
        '';
      };

      match = lib.mkOption {
        type = lib.types.listOf udevForwarderDomainMatchOpts;
        default = [ ];
      };
    };
  };
in
{
  options = {
    vfio = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Enable support for VFIO.
        '';
      };

      iommuType = lib.mkOption {
        type = lib.types.enum [
          "intel"
          "intel_sm"
          "amd"
        ];
        description = ''
          Sets the IOMMU type.
        '';
      };

      devices = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          List of PCIe device ID retrievable by running `lspci`
          then query device ID with `lspci -n -s nn:mm` where
          nn:mm is the bus ID (e.g. 03:00)
        '';
      };

      nested = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Enable nested virtualization support.
          '';
        };
      };

      looking-glass = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Enable Looking Glass SHM support.
          '';
        };
      };

      hugepages = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Reserve 1 GiB hugepages at boot for VMs.
          '';
        };

        count = lib.mkOption {
          type = lib.types.ints.positive;
          default = 1;
          description = ''
            Number of 1 GiB hugepages to reserve.
          '';
        };
      };

      udev-forwarder = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Enable dynamic USB forwarding via udev-forwarder.
          '';
        };

        config = {
          uri = lib.mkOption {
            type = lib.types.str;
            default = "qemu:///system";
            description = ''
              Specify the Libvirt URI to connect to.
            '';
          };

          domain = lib.mkOption {
            type = lib.types.listOf udevForwarderDomainOpts;
            default = [ ];
            description = "List of VMs and its passthrough devices.";
          };
        };
      };
    };
  };

  config = lib.mkIf config.vfio.enable {
    boot.kernelParams = [
      "iommu=pt"
    ]
    ++

      (
        if cfg.iommuType == "intel" then
          [ "intel_iommu=on" ]
        else if
          cfg.iommuType == "intel_sm" # For Xeons with DSA.
        then
          [ "intel_iommu=on,sm_on" ]
        else
          [ "amd_iommu=on" ]
      )
    ++
    (lib.optionals cfg.hugepages.enable [
      "default_hugepagesz=1G"
      "hugepagesz=1G"
      "hugepages=${builtins.toString cfg.hugepages.count}"
    ]);

    environment.etc = lib.mkMerge [
      {
        "modprobe.d/kvm.conf" = {
          text = ''
            options kvm ignore_msrs=1
            options kvm report_ignored_msrs=0
          '';
        };
      }
      (lib.mkIf (cfg.nested.enable && cfg.iommuType == "intel") {
        "modprobe.d/kvm_intel.conf" = {
          text = ''
            options kvm_intel nested=1
            options kvm_intel enable_shadow_vmcs=1
            options kvm_intel enable_apicv=1
            options kvm_intel ept=1
          '';
        };
      })
      (lib.mkIf (cfg.nested.enable && cfg.iommuType == "amd") {
        "modprobe.d/kvm_amd.conf" = {
          text = ''
            options kvm_amd nested=1
          '';
        };
      })
      (lib.mkIf (lib.length cfg.devices > 0) {
        "modprobe.d/vfio.conf" = {
          text = ''
            options vfio-pci ids=${lib.concatStringsSep "," cfg.devices} disable_vga=1
            softdep amdgpu pre: vfio-pci
            softdep drm pre: vfio-pci
            softdep i915 pre: vfio-pci
            softdep nouveau pre: vfio-pci
            softdep radeon pre: vfio-pci
          '';
        };
      })
      (lib.mkIf (cfg.udev-forwarder.enable) {
        "udev-forwarder/config.toml" = {
          source = (tomlFormat.generate "config.toml" cfg.udev-forwarder.config);
        };
      })
    ];

    boot.kernelModules = [
      "vfio_pci"
      "vfio_iommu_type1"
      "vfio"
    ];

    boot.initrd.kernelModules = [
      "vfio_pci"
      "vfio_iommu_type1"
      "vfio"
    ];

    boot.initrd.extraFiles =
      let
        mkExtraInitrd =
          f:
          (lib.mkIf (lib.hasAttr f config.environment.etc) {
            "/etc/${f}".source = config.environment.etc."${f}".source;
          });
      in
      lib.mkMerge [ (mkExtraInitrd "modprobe.d/vfio.conf") ];

    systemd.services = lib.mkMerge [
      (lib.mkIf cfg.udev-forwarder.enable {
        "udev-forwarder" = {
          enable = true;
          wants = [
            "systemd-udevd.service"
            "libvirtd.service"
          ];
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.local.udev-forwarder}/bin/udev-forwarder /etc/udev-forwarder/config.toml";
          };
        };
      })
    ];

    systemd.tmpfiles.settings = lib.mkIf cfg.looking-glass.enable {
      "looking-glass" = {
        "/dev/shm/looking-glass" = {
          f = {
            group = if config.virtualisation.libvirtd.enable then "libvirtd" else "kvm";

            mode = "0770";
            user = "root";
          };
        };
      };
    };
  };
}
