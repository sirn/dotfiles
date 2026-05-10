final: prev: inputs:
let
  inherit (prev) callPackage;
  inherit (prev.lib) recurseIntoAttrs;
in
{
  mitsuhiko-agent-stuff = (callPackage ./by-name/mitsuhiko-agent-stuff { });
  skill-ast-grep = (callPackage ./by-name/skill-ast-grep { });

  asdcontrol = (callPackage ./by-name/asdcontrol { });

  coincide = (callPackage ./by-name/coincide { });

  emacsPackages = {
    phscroll = (callPackage ./by-name/emacs/elisp-packages/phscroll/package.nix { });
  };

  envWrapper = (callPackage ./by-name/env-wrapper/package.nix { });

  omniwm = (callPackage ./by-name/omniwm/package.nix { });

  pi-hashline-edit = (callPackage ./by-name/pi-hashline-edit/package.nix { });

  node-textfile-collector-scripts = (
    callPackage ./by-name/prometheus/node-textfile-collector-scripts.nix { }
  );

  repoman = (callPackage ./by-name/repoman/package.nix { });

  tincan = (callPackage ./by-name/tincan/package.nix { });

  udev-forwarder = (callPackage ./by-name/udev-forwarder { });

  lsp-cli = (callPackage ./by-name/lsp-cli/package.nix { });

  web-cli = (callPackage ./by-name/web-cli/package.nix { });

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });
}
