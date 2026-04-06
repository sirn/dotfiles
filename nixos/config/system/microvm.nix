{ microvm, ... }:

{
  imports = [ microvm.nixosModules.host ];

  nix.settings = {
    substituters = [ "https://microvm.cachix.org" ];
    trusted-public-keys = [ "microvm.cachix.org-1:oXnBc6hRE3eX5rSYdRyMYXnfzcCxC7yKPTbZXALsqys=" ];
  };
}
