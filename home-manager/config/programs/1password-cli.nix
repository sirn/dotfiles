{ pkgs, ... }:

{
  assertions = [
    {
      assertion = pkgs.stdenv.isDarwin;
      message = "1Password CLI must be installed with the NixOS module programs._1password on Linux.";
    }
  ];

  home.packages = [ pkgs._1password-cli ];
}
