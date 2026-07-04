{ pkgs, ... }:

{ environment.systemPackages = with pkgs; [ attic-client ]; }
