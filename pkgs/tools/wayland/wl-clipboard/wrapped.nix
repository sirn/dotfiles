{ stdenvNoCC, lib, makeWrapper, wl-clipboard, coreutils, ... }:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "wl-clipboard-wrapped";
  inherit (wl-clipboard) version;

  dontUnpack = true;
  dontBuild = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    makeWrapper "${wl-clipboard}/bin/wl-copy" "$out/bin/wl-copy" \
      --prefix PATH : ${lib.makeBinPath [ coreutils ]}
    makeWrapper "${wl-clipboard}/bin/wl-paste" "$out/bin/wl-paste" \
      --prefix PATH : ${lib.makeBinPath [ coreutils ]}
  '';
})
