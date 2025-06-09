{ config, ... }:

let
  dropboxDir = "${config.home.homeDirectory}/Dropbox";
in
{
  home.file = {
    "Org/" = {
      source = config.lib.file.mkOutOfStoreSymlink "${dropboxDir}/Org";
    };
  };
}
