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

  mcpServers = {
    mcp-remote = (callPackage ./by-name/mcp-servers/mcp-remote/package.nix { });
  };

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });

  tincan = (callPackage ./by-name/tincan/package.nix { });

  repoman = (callPackage ./by-name/repoman/package.nix { });

  claude-code-bin = (callPackage ./by-name/claude-code-bin/package.nix { });

  claude-code-seccomp = (callPackage ./by-name/claude-code-seccomp/package.nix { });

  codex-bin = (callPackage ./by-name/codex-bin/package.nix { });

  gemini-cli-bin = (callPackage ./by-name/gemini-cli-bin/package.nix { });

  opencode-bin = (callPackage ./by-name/opencode-bin/package.nix { });

  pi-coding-agent-bin = (callPackage ./by-name/pi-coding-agent-bin/package.nix { });
}
