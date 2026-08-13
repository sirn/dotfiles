{ inputs, nixpkgsConfig }:
let
  compatOverlays = [
    # Bun-compiled tailwindcss standalone is only linker-signed, which
    # amfid kills on recent macOS; trunk's tailwind hook then can't read
    # its version offline.
    #
    # TODO: revisit after macOS 27 is released.
    (
      final: prev:
      prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
        tailwindcss_4 = prev.tailwindcss_4.overrideAttrs (
          final': prev': {
            postFixup = (prev'.postFixup or "") + ''
              /usr/bin/codesign -f -s - $out/bin/.tailwindcss-wrapped
            '';
          }
        );
      }
    )
  ];
in
compatOverlays
++ [
  inputs.nixgl.overlay

  (final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      config = nixpkgsConfig;
      overlays = compatOverlays;
    };

    nur = import inputs.nur {
      nurpkgs = final;
      pkgs = final;
    };

    llm-agents =
      let
        raw = inputs.llm-agents.packages.${final.stdenv.hostPlatform.system};
      in
      raw
      // {
        # pi ships a Bun-compiled libexec/pi/pi native binary that is only
        # linker-signed; nix's fixup invalidates the signature and amfid
        # kills it (silent SIGKILL on aarch64-darwin). Re-sign ad-hoc.
        pi =
          if final.stdenv.hostPlatform.isDarwin then
            raw.pi.overrideAttrs (
              _: prev: {
                postFixup = (prev.postFixup or "") + ''
                  /usr/bin/codesign -f -s - $out/libexec/pi/pi
                '';
              }
            )
          else
            raw.pi;
      };

    local = (import ./default.nix final prev inputs).${final.stdenv.hostPlatform.system};
  })
]
