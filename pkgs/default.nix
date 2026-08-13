final: prev: inputs:
let
  inherit (prev) callPackage;

  commonPackages = {
    asdcontrol = (callPackage ./by-name/asdcontrol { });

    coincide = (callPackage ./by-name/coincide { });

    coord = (callPackage ./by-name/coord/package.nix { });

    handsfree = (callPackage ./by-name/handsfree/package.nix { });

    lofi = (callPackage ./by-name/lofi/package.nix { });

    emacsPackages.phscroll = (callPackage ./by-name/emacs/elisp-packages/phscroll/package.nix { });

    context7-cli = (callPackage ./by-name/context7-cli/package.nix { });

    exa-cli = (callPackage ./by-name/exa-cli/package.nix { });

    fizzterm = (callPackage ./by-name/fizzterm/package.nix { });

    powerband = (callPackage ./by-name/powerband { });

    pi-vcc = (callPackage ./by-name/pi-vcc/package.nix { });

    node-textfile-collector-scripts = (
      callPackage ./by-name/prometheus/node-textfile-collector-scripts.nix { }
    );

    repoman = (callPackage ./by-name/repoman/package.nix { });

    skill-ast-grep = (callPackage ./by-name/skill-ast-grep { });

    skill-tuicr = (callPackage ./by-name/skill-tuicr { });

    tincan = (callPackage ./by-name/tincan/package.nix { });

    tiler = (callPackage ./by-name/tiler/package.nix { });

    lsp-cli = (callPackage ./by-name/lsp-cli/package.nix { });

    wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });
  };

  linuxPackages = commonPackages // {
    helium = (callPackage ./by-name/helium/package.nix { });

    udev-forwarder = (callPackage ./by-name/udev-forwarder { });
  };
in
{
  x86_64-linux = linuxPackages;
  aarch64-linux = linuxPackages;
  x86_64-darwin = commonPackages;
  aarch64-darwin = commonPackages;
}
