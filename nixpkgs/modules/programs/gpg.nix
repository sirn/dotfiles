{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isLinux isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";

  pinentryProgram =
    if config.machine.gui.enable && isLinux then
      "${pkgs.pinentry-qt}/bin/pinentry-qt"
    else
      if config.machine.gui.enable && isDarwin then
        "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
      else
        "${pkgs.pinentry}/bin/pinentry";
in
{
  programs.gpg = {
    enable = true;
    settings = {
      default-key = "0x4AFE17C811D18D20";
      local-user = [
        "0x8FB3641FEB97EE77!"
        "0x45FD642A9664BCDE!"
      ];

      no-secmem-warning = true;
      keyserver = "hkps://keys.openpgp.org";
      use-agent = true;
    };
  };

  home.sessionVariablesExtra = ''
    unset SSH_AGENT_PID
    unset SSH_AUTH_SOCK

    if [ "''${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
      SSH_AUTH_SOCK="''$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)"
      export SSH_AUTH_SOCK
    fi
  '';

  home.file = {
    ".gnupg/sshcontrol" = {
      text = ''
        095FC3D3CC0EC41DDBDD0D33543EF69A4743F949
        51D2F7BE8DE93487063F9089BEBAA4C940660D18
      '';
    };
    ".gnupg/gpg-agent.conf" = {
      text = ''
        # Pinentry
        allow-emacs-pinentry
        allow-loopback-pinentry
        pinentry-program ${pinentryProgram}

        # TTL
        default-cache-ttl-ssh 86400
        default-cache-ttl 86400
        max-cache-ttl-ssh 604800
        max-cache-ttl 604800

        # SSH
        enable-ssh-support
        ssh-fingerprint-digest SHA256
      '';
    };
    ".gnupg/pubring.kbx" = {
      source = mkOutOfStoreSymlink "${dotprivDir}/etc/gnupg/pubring.kbx";
    };
    ".gnupg/trustdb.gpg" = {
      source = mkOutOfStoreSymlink "${dotprivDir}/etc/gnupg/trustdb.gpg";
    };
    ".gnupg/tofu.db" = {
      source = mkOutOfStoreSymlink "${dotprivDir}/etc/gnupg/tofu.db";
    };
    ".gnupg/private-keys-v1.d" = {
      recursive = true;
      source = mkOutOfStoreSymlink "${dotprivDir}/etc/gnupg/private-keys-v1.d";
    };
  };
}
