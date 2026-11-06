{ pkgs, ... }:

{ home.packages = with pkgs; [ local.zrex ]; }
