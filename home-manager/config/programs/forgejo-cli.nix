{ pkgs, ... }:

{ home.packages = with pkgs; [ forgejo-cli ]; }
