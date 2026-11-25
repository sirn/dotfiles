{
  microvm,
  home-manager,
  direnv-op,
  sops-nix,
  niri,
  nix-index-database,
  noctalia,
  overlays,
  stateVersion,
  modules,
}:

{
  hostname,
  vcpu ? 8,
  mem ? 16384,
  extraConfig ? { },

  extraModules ? [ ],

  # Networking
  interfaceType ? "user",
  interfaces ? [ ],
  mac ? "",

  # Optionals
  useHomeManager ? false,
  extraHomeModules ? [ ],
  hmUsername ? "sirn",

  # Nix Store
  useNixStoreOverlay ? false,
  nixStoreOverlaySize ? 65536,
  nixStoreOverlayImage ? "/var/lib/microvms/${hostname}/nix-store-overlay.img",

  # Home Mount
  useHomeMount ? false,
  homeMountSource ? "/var/lib/microvms/${hostname}/home",

  # Var Mount
  useVarMount ? true,
  varMountSource ? "/var/lib/microvms/${hostname}/var",

  # Containers Volume
  useContainersVolume ? false,
  containersVolumeSize ? 65536,
  containersVolumeImage ? "/var/lib/microvms/${hostname}/containers.img",
}:

let
  nixosConfig = modules.nixos.config;
  hmConfig = modules.homeManager.config;
in
{
  # A VM is evaluated as its own nixosSystem, so it needs the registry again.
  specialArgs = { inherit nixosConfig; };

  config = { lib, ... }: {
    imports = [
      microvm.nixosModules.microvm
      sops-nix.nixosModules.sops
    ]
    ++ extraModules
    ++ lib.optionals useHomeManager [
      home-manager.nixosModules.home-manager
      (
        { lib, pkgs, ... }:
        let
          hm-backup = pkgs.writeScriptBin "hm-backup" ''
            #!${pkgs.runtimeShell}
            mv "$1" "$1.backup.$(date +%s)"
          '';
        in
        {
          home-manager.backupCommand = lib.getExe hm-backup;
          home-manager.extraSpecialArgs = { inherit hmConfig; };
          home-manager.users.${hmUsername} = {
            nixpkgs.overlays = overlays;
            nixpkgs.config.allowUnfree = true;
            home.stateVersion = stateVersion;
          };
        }
      )
    ]
    ++ lib.optionals useHomeManager [
      {
        home-manager.users.${hmUsername} = {
          imports = [
            direnv-op.homeModules.default
            niri.homeModules.niri
            nix-index-database.homeModules.nix-index
            noctalia.homeModules.default
            sops-nix.homeManagerModules.sops
            ../home-manager/modules
          ]
          ++ extraHomeModules;
        };
      }
    ]
    ++ lib.optionals (useHomeManager && useContainersVolume) [
      {
        home-manager.users.${hmUsername} = {
          services.podman.settings.storage = {
            storage.graphroot = "/var/lib/containers/user/${hmUsername}/storage";
          };
        };
      }
    ];

    nixpkgs.overlays = overlays;
    system.stateVersion = stateVersion;
    networking.hostName = hostname;

    # Regenerate hostId because a guest may not import common.nix.
    networking.hostId = builtins.substring 0 8 (builtins.hashString "sha256" hostname);

    microvm = {
      hypervisor = "qemu";
      inherit vcpu mem;

      interfaces =
        if (interfaces != [ ]) then
          interfaces
        else
          [
            {
              type = interfaceType;
              id = "vm-${builtins.substring 3 11 hostname}";
              mac =
                if mac != "" then
                  mac
                else
                  let
                    hash = builtins.hashString "sha256" hostname;
                    take2 = off: builtins.substring off 2 hash;
                  in
                  "02:${take2 0}:${take2 2}:${take2 4}:${take2 6}:${take2 8}";
            }
          ];

      virtiofsd = {
        inodeFileHandles = "prefer";
        threadPoolSize = 0;
        extraArgs = [
          "--cache=always"
          "--writeback"
          "--announce-submounts"
        ];
      };

      shares = [
        {
          proto = "virtiofs";
          tag = "ro-store";
          source = "/nix/store";
          mountPoint = "/nix/.ro-store";
        }
      ]
      ++ lib.optionals useHomeMount [
        {
          proto = "virtiofs";
          tag = "home";
          source = homeMountSource;
          mountPoint = "/home";
        }
      ]
      ++ lib.optionals useVarMount [
        {
          proto = "virtiofs";
          tag = "var";
          source = varMountSource;
          mountPoint = "/var";
        }
      ];

      preStart = lib.optionalString useNixStoreOverlay ''
        rm -f "${nixStoreOverlayImage}"
      '';

      writableStoreOverlay = if useNixStoreOverlay then "/nix/.rw-store" else null;

      volumes =
        lib.optionals useNixStoreOverlay [
          {
            image = nixStoreOverlayImage;
            mountPoint = "/nix/.rw-store";
            size = nixStoreOverlaySize;
          }
        ]
        ++ lib.optionals useContainersVolume [
          {
            image = containersVolumeImage;
            mountPoint = "/var/lib/containers";
            size = containersVolumeSize;
          }
        ];
    }
    // extraConfig;

    # When /var is mounted, store SSH host key in a persistent location.
    services.openssh.hostKeys = lib.optionals useVarMount [
      {
        path = "/var/lib/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
      {
        path = "/var/lib/ssh/ssh_host_rsa_key";
        type = "rsa";
        bits = 4096;
      }
    ];

    systemd.services.setup-podman-storage = lib.mkIf useContainersVolume {
      description = "Setup podman storage directories";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        if [ ! -d "/var/lib/containers/user/${hmUsername}" ]; then
          mkdir -p "/var/lib/containers/user/${hmUsername}/storage/tmp"
          chown -R "${hmUsername}:${hmUsername}" "/var/lib/containers/user/${hmUsername}"
          chmod 0755 "/var/lib/containers/user/${hmUsername}"
          chmod 0700 "/var/lib/containers/user/${hmUsername}/storage"
          chmod 0700 "/var/lib/containers/user/${hmUsername}/storage/tmp"
        fi
      '';
    };

    # Configure podman/buildah to use container storage for temporary files
    # instead of /var/tmp (which has permission issues in MicroVMs)
    virtualisation.containers.enable = lib.mkDefault useContainersVolume;
    virtualisation.containers.containersConf.settings = lib.mkIf useContainersVolume {
      engine.image_copy_tmp_dir = "storage";
    };
  };
}
