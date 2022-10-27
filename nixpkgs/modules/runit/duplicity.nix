{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (config.home) username;

  gpgKey = config.programs.gpg.settings.default-key;
  homeDir = config.home.homeDirectory;
  dotprivDir = "${homeDir}/.dotpriv";
in
{
  home = {
    file = {
      ".local/var/service/duplicity/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.openssh}/bin:${pkgs.execline}/bin:${pkgs.busybox}/bin

          backtick -n -E uid { id -u }
          define xdg-runtime-dir /run/user/''${uid}

          export HOME ${homeDir}
          export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
          export SSH_AUTH_SOCK ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh

          define -s duplicity "${pkgs.duplicity}/bin/duplicity --gpg-binary=${pkgs.gnupg}/bin/gpg2 --encrypt-key=${gpgKey} --use-agent"

          fdmove -c 2 1
          backtick -n -E target { ${dotprivDir}/libexec/duplicity/target_cmd.sh }
          if { test -d ''${xdg-runtime-dir} }
          foreground { mkdir -p ${homeDir}/.local/var/run }
          ${pkgs.snooze}/bin/snooze -v -R 10m -s 1h -H/1 -t ${homeDir}/.local/var/run/duplicity_timefile
          nice -n 20

          if {
              ''${duplicity}
                  --full-if-older-than=1M
                  --allow-source-mismatch
                  --include-filelist="${dotprivDir}/var/duplicity/filelist.txt"
                  ${homeDir}
                  ''${target}
          }

          foreground { touch ${homeDir}/.local/var/run/duplicity_timefile }
          foreground { ''${duplicity} --force remove-all-inc-of-but-n-full 3 ''${target} }
          foreground { ''${duplicity} --force remove-older-than 24M ''${target} }
        '';
      };
      ".local/var/service/duplicity/log/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
          define logpath ${homeDir}/.local/var/log/duplicity

          if { mkdir -p ''${logpath} }
          ${pkgs.s6}/bin/s6-log -b n10 s1000000 t !"${pkgs.gzip}/bin/gzip -nq9" ''${logpath}
        '';
      };
    };

    # Runit only readlink for one level; mkOutOfStoreSymlink creates nested
    # symlinks of src->/nix/store->/nix/store/hm_...->dest which confuse runit
    activation = {
      duplicitySuperviseActivation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.local/var/service/duplicity/log"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.duplicity" "$HOME/.local/var/service/duplicity/supervise"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.duplicity-log" "$HOME/.local/var/service/duplicity/log/supervise"
      '';
    };
  };
}
