final: prev: inputs:
let
  inherit (prev) callPackage;

  commonPackages = {
    asdcontrol = (callPackage ./by-name/asdcontrol/package.nix { });

    ast-grep = (
      callPackage ./by-name/ast-grep-skills/package.nix {
        ast-grep = final.unstable.ast-grep;
      }
    );

    coincide = (callPackage ./by-name/coincide/package.nix { });

    coord = (callPackage ./by-name/coord/package.nix { });

    handsfree = (callPackage ./by-name/handsfree/package.nix { });

    lofi = (callPackage ./by-name/lofi/package.nix { });

    emacsPackages.phscroll = (callPackage ./by-name/phscroll/package.nix { });

    context7-cli = (callPackage ./by-name/context7-cli/package.nix { });

    exa-cli = (callPackage ./by-name/exa-cli/package.nix { });

    fizzterm = (callPackage ./by-name/fizzterm/package.nix { });

    powerband = (callPackage ./by-name/powerband/package.nix { });

    pi-vcc = (callPackage ./by-name/pi-vcc/package.nix { });

    node-textfile-collector-scripts = (
      callPackage ./by-name/node-textfile-collector-scripts/package.nix { }
    );

    repoman = (callPackage ./by-name/repoman/package.nix { });

    skill-tuicr = (callPackage ./by-name/skill-tuicr/package.nix { });

    tincan = (callPackage ./by-name/tincan/package.nix { });

    tiler = (callPackage ./by-name/tiler/package.nix { });

    lsp-cli = (callPackage ./by-name/lsp-cli/package.nix { });

    wrapped-uv = (callPackage ./by-name/wrapped-uv/package.nix { });
  };

  linuxPackages = commonPackages // {
    helium = (callPackage ./by-name/helium/package.nix { });

    udev-forwarder = (callPackage ./by-name/udev-forwarder/package.nix { });
  };
in
{
  x86_64-linux = linuxPackages;
  aarch64-linux = linuxPackages;
  x86_64-darwin = commonPackages;
  aarch64-darwin = commonPackages;
}
