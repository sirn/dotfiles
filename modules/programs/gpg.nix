{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isLinux isDarwin;

  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
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

  home.file = {
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
