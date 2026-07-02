final: prev: inputs:
let
  inherit (prev) callPackage;

  commonPackages = {
    asdcontrol = (callPackage ./by-name/asdcontrol { });

    coincide = (callPackage ./by-name/coincide { });

    coord = (callPackage ./by-name/coord/package.nix { });

    emacsPackages.phscroll = (callPackage ./by-name/emacs/elisp-packages/phscroll/package.nix { });

    envWrapper = (callPackage ./by-name/env-wrapper/package.nix { });

    context7-cli = (callPackage ./by-name/context7-cli/package.nix { });

    exa-cli = (callPackage ./by-name/exa-cli/package.nix { });

    portless = (callPackage ./by-name/portless/package.nix { });

    powerband = (callPackage ./by-name/powerband { });

    pi-vcc = (callPackage ./by-name/pi-vcc/package.nix { });

    pi-tool-repair = (callPackage ./by-name/pi-tool-repair/package.nix { });

    mitsuhiko-agent-stuff = (callPackage ./by-name/mitsuhiko-agent-stuff { });

    node-textfile-collector-scripts = (
      callPackage ./by-name/prometheus/node-textfile-collector-scripts.nix { }
    );

    repoman = (callPackage ./by-name/repoman/package.nix { });

    skill-ast-grep = (callPackage ./by-name/skill-ast-grep { });

    skill-tuicr = (callPackage ./by-name/skill-tuicr { });

    tincan = (callPackage ./by-name/tincan/package.nix { });

    tiler = (callPackage ./by-name/tiler/package.nix { });

    terminal-use = (callPackage ./by-name/terminal-use/package.nix { });

    lsp-cli = (callPackage ./by-name/lsp-cli/package.nix { });

    localterm = (callPackage ./by-name/localterm/package.nix { });

    wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });
  };

  linuxPackages = commonPackages // {
    udev-forwarder = (callPackage ./by-name/udev-forwarder { });
  };
in
{
  x86_64-linux = linuxPackages;
  aarch64-linux = linuxPackages;
  x86_64-darwin = commonPackages;
  aarch64-darwin = commonPackages;
}
