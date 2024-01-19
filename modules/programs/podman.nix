{ pkgs, ... }:

{
  home.packages = with pkgs; [
    podman
    skopeo
  ];
}
