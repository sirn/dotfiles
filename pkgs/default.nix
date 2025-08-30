{ lib, pkgs, ... }:

let
  inherit (lib) callPackageWith recurseIntoAttrs;

  callPackage = callPackageWith (pkgs);

  callPackageUnstable = callPackageWith (pkgs.unstable);
in
{
  inherit (recurseIntoAttrs (callPackage ./by-name/ia-fonts { }))
    ia-writer-duo-static
    ia-writer-mono-static
    ia-writer-quattro-static;

  emacsPackages = {
    sqlite3 = (callPackage ./by-name/emacs/elisp-packages/sqlite3 { });
  };

  envWrapper = (callPackage ./by-name/env-wrapper { });

  firefox-csshacks = (callPackage ./by-name/firefox/csshacks.nix { });

  mcpServers = {
    context7 = (callPackage ./by-name/mcp-servers/context7 { });

    brave-search = (callPackage ./by-name/mcp-servers/brave-search { });
  };

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });
}
