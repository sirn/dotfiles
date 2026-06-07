final: prev: inputs:
let
  inherit (prev) callPackage;
  inherit (prev.lib) recurseIntoAttrs;
in
{
  asdcontrol = (callPackage ./by-name/asdcontrol { });

  coincide = (callPackage ./by-name/coincide { });

  coord = (callPackage ./by-name/coord/package.nix { });

  claude-code = (callPackage ./by-name/claude-code/package.nix { });

  emacsPackages = {
    phscroll = (callPackage ./by-name/emacs/elisp-packages/phscroll/package.nix { });
  };

  envWrapper = (callPackage ./by-name/env-wrapper/package.nix { });

  context7-cli = (callPackage ./by-name/context7-cli/package.nix { });

  exa-cli = (callPackage ./by-name/exa-cli/package.nix { });

  powerband = (callPackage ./by-name/powerband { });

  pi-coding-agent = (callPackage ./by-name/pi-coding-agent/package.nix { });

  pi-hashline-edit = (callPackage ./by-name/pi-hashline-edit/package.nix { });

  pi-vcc = (callPackage ./by-name/pi-vcc/package.nix { });

  mitsuhiko-agent-stuff = (callPackage ./by-name/mitsuhiko-agent-stuff { });

  mouseless = (callPackage ./by-name/mouseless/package.nix { });

  node-textfile-collector-scripts = (
    callPackage ./by-name/prometheus/node-textfile-collector-scripts.nix { }
  );

  repoman = (callPackage ./by-name/repoman/package.nix { });

  skill-ast-grep = (callPackage ./by-name/skill-ast-grep { });

  tincan = (callPackage ./by-name/tincan/package.nix { });

  udev-forwarder = (callPackage ./by-name/udev-forwarder { });

  lsp-cli = (callPackage ./by-name/lsp-cli/package.nix { });

  web-cli = (callPackage ./by-name/web-cli/package.nix { });

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });
}
