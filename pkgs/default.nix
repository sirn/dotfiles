final: prev: inputs:
let
  inherit (prev) callPackage;
  inherit (prev.lib) recurseIntoAttrs;
in
{
  asdcontrol = (callPackage ./by-name/asdcontrol { });

  claude-code-seccomp = (callPackage ./by-name/claude-code-seccomp/package.nix { });

  emacsPackages = {
    phscroll = (callPackage ./by-name/emacs/elisp-packages/phscroll/package.nix { });
  };

  envWrapper = (callPackage ./by-name/env-wrapper/package.nix { });

  node-textfile-collector-scripts = (
    callPackage ./by-name/prometheus/node-textfile-collector-scripts.nix { }
  );

  repoman = (callPackage ./by-name/repoman/package.nix { });

  tincan = (callPackage ./by-name/tincan/package.nix { });

  udev-forwarder = (callPackage ./by-name/udev-forwarder { });

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });

  wayland-protocols-git = (callPackage ./by-name/wayland-protocols-git { });

  wlroots-git = (
    callPackage ./by-name/wlroots-git { wayland-protocols = final.local.wayland-protocols-git; }
  );

  sway-unwrapped-git = (
    callPackage ./by-name/sway-unwrapped-git {
      wlroots = final.local.wlroots-git;
      wayland-protocols = final.local.wayland-protocols-git;
      nixpkgsPath = inputs.nixpkgs;
    }
  );

  sway-git = prev.sway.override { sway-unwrapped = final.local.sway-unwrapped-git; };
}
