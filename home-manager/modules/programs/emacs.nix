{ lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  options.programs.emacs = {
    afterInitExtra = mkOption {
      type = types.lines;
      default = "";
      description = "Extra Elisp to run on gemacs-after-init-hook.";
    };

    themePackages = mkOption {
      type = types.functionTo (types.listOf types.package);
      default = epkgs: [ ];
      description = "Emacs packages for the color theme.";
    };
  };
}
