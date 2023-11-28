{ config, lib, pkgs, ... }:

let
  inherit (import ./utils.nix { inherit config lib pkgs; }) mkRunit;
in
mkRunit {
  rname = "runit";
  serviceDir = ".local/var/service";
  logDir = ".local/var/log";
}
