{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    argocd
    helmfile
    kubernetes-helm
    kustomize
  ];
}
