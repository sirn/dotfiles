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

  inherit (recurseIntoAttrs (callPackage ./by-name/ia-fonts/package.nix { }))
    ia-writer-duo-static
    ia-writer-mono-static
    ia-writer-quattro-static
    ;

  nvidia-gpu-exporter = (callPackage ./by-name/prometheus/nvidia-gpu-exporter.nix { });

  node-textfile-collector-scripts = (
    callPackage ./by-name/prometheus/node-textfile-collector-scripts.nix { }
  );

  pi-better-messages-cache = (callPackage ./by-name/pi-better-messages-cache/package.nix { });

  repoman = (callPackage ./by-name/repoman/package.nix { });

  tincan = (callPackage ./by-name/tincan/package.nix { });

  udev-forwarder = (callPackage ./by-name/udev-forwarder { });

  vulkan-hdr-layer = (callPackage ./by-name/vulkan-hdr-layer { });

  wrapped-uv = (callPackage ./by-name/wrapped-uv/wrapped.nix { });
}
