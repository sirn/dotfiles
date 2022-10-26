{ config, pkgs, ... }:

let
  localNodePackages = pkgs.callPackage ./node-packages { };
in
{
  home.packages = with pkgs; [
    nodejs
    nodePackages.jsonlint
    nodePackages.node2nix
    nodePackages.npm
    nodePackages.pnpm
    nodePackages.typescript
    nodePackages.typescript-language-server

    localNodePackages.eslint
    localNodePackages.eslint-config-prettier
    localNodePackages.eslint-plugin-svelte3
    localNodePackages.prettier
    localNodePackages.prettier-plugin-svelte
    localNodePackages.stylelint
    localNodePackages.stylelint-config-recommended
    localNodePackages.stylelint-scss
  ];
}
