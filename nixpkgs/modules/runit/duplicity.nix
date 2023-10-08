{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (config.home) username homeDirectory;

  gpgKey = config.programs.gpg.settings.default-key;
  dotprivDir = "${homeDirectory}/.dotpriv";
in
{
  runit.services = {
    duplicity = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.openssh}/bin:${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        backtick -n -E uid { id -u }
        define xdg-runtime-dir /run/user/''${uid}
        define -s duplicity "${pkgs.duplicity}/bin/duplicity --gpg-binary=${pkgs.gnupg}/bin/gpg2 --encrypt-key=${gpgKey} --use-agent"

        export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
        export SSH_AUTH_SOCK ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh

        fdmove -c 2 1
        backtick -n -E target { ${dotprivDir}/libexec/duplicity/target_cmd.sh }
        if { test -d ''${xdg-runtime-dir} }
        foreground { mkdir -p ${homeDirectory}/.local/var/run }
        ${pkgs.snooze}/bin/snooze -v -R 10m -s 1h -H/1 -t ${homeDirectory}/.local/var/run/duplicity_timefile
        nice -n 20

        if {
            ''${duplicity}
                --full-if-older-than=1M
                --allow-source-mismatch
                --include-filelist="${dotprivDir}/var/duplicity/filelist.txt"
                ${homeDirectory}
                ''${target}
        }

        foreground { touch ${homeDirectory}/.local/var/run/duplicity_timefile }
        foreground { ''${duplicity} --force remove-all-inc-of-but-n-full 3 ''${target} }
        foreground { ''${duplicity} --force remove-older-than 24M ''${target} }
      '';
    };
  };
}
