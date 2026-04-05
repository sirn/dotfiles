{ config, pkgs, ... }:

{
  programs.aria2 = {
    enable = true;
    settings = {
      continue = true;
      max-connection-per-server = 8;
      min-split-size = "1M";
    };
  };
}
