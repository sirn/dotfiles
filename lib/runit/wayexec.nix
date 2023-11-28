{ config, lib, pkgs, ... }:

let
  inherit (import ./utils.nix { inherit config lib pkgs; }) mkRunit;
in
mkRunit {
  rname = "wayexec";
  serviceDir = ".local/var/wayexec";
  logDir = ".local/var/log";
}
