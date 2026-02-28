{ config, lib, pkgs, ... }:

{
  programs.mcp = {
    enable = true;
    servers = { };
  };
}
