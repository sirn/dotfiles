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
        "0xB8B8BEB8DCA7A542!"
        "0x865D6A23205F35CA!"
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
