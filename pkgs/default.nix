final: prev:
let
  inherit (prev) callPackage;
  inherit (prev.lib) recurseIntoAttrs;
in
{
  inherit (recurseIntoAttrs (callPackage ./by-name/ia-fonts/package.nix { }))
    ia-writer-duo-static
    ia-writer-mono-static
    ia-writer-quattro-static;

  emacsPackages = {
    sqlite3 = (callPackage ./by-name/emacs/elisp-packages/sqlite3/package.nix { });
  };

  envWrapper = (callPackage ./by-name/env-wrapper/package.nix { });

  firefox-csshacks = (callPackage ./by-name/firefox/csshacks.nix { });

  mcpServers = {
    context7 = (callPackage ./by-name/mcp-servers/context7/package.nix { });

    brave-search = (callPackage ./by-name/mcp-servers/brave-search/package.nix { });

    fetch = (callPackage ./by-name/mcp-servers/fetch/package.nix { });
  };

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });

  octofriend = (callPackage ./by-name/octofriend/package.nix { });
}
