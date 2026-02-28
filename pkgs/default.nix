final: prev: inputs:
let
  inherit (prev) callPackage;
  inherit (prev.lib) recurseIntoAttrs;
in
{
  inherit (recurseIntoAttrs (callPackage ./by-name/ia-fonts/package.nix { }))
    ia-writer-duo-static
    ia-writer-mono-static
    ia-writer-quattro-static
    ;

  emacsPackages = {
    sqlite3 = (callPackage ./by-name/emacs/elisp-packages/sqlite3/package.nix { });
    phscroll = (callPackage ./by-name/emacs/elisp-packages/phscroll/package.nix { });
  };

  envWrapper = (callPackage ./by-name/env-wrapper/package.nix { });

  firefox-csshacks = (callPackage ./by-name/firefox/csshacks.nix { });

  mcpServers = {
    context7 = (callPackage ./by-name/mcp-servers/context7/package.nix { });

    mcp-remote = (callPackage ./by-name/mcp-servers/mcp-remote/package.nix { });
  };

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });

  tincan = (callPackage ./by-name/tincan/package.nix { });

  repoman = (callPackage ./by-name/repoman/package.nix { });

  pi-coding-agent = (callPackage ./by-name/pi-coding-agent/package.nix { });
}
