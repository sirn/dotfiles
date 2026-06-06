{
  lib,
  stdenv,
  fetchurl,
  appimageTools,
  undmg,
  makeWrapper,
  webkitgtk_4_1,
  mesa,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
  appName = "Mouseless.app";

  platformData =
    sources.${stdenv.hostPlatform.system}
      or (throw "mouseless: unsupported platform ${stdenv.hostPlatform.system}");

  src = fetchurl {
    url = platformData.url;
    hash = platformData.hash;
  };

  meta = with lib; {
    description = "Keyboard-driven mouse control";
    homepage = "https://mouseless.click";
    license = licenses.unfree;
    sourceProvenance = [ sourceTypes.binaryNativeCode ];
    platforms = builtins.attrNames (removeAttrs sources [ "version" ]);
    mainProgram = "mouseless";
  };

  webkit-debian =
    pkgs:
    pkgs.runCommand "webkit-debian" { } ''
      mkdir -p $out/lib/x86_64-linux-gnu/webkit2gtk-4.1
      cp -r ${webkitgtk_4_1}/libexec/webkit2gtk-4.1/* $out/lib/x86_64-linux-gnu/webkit2gtk-4.1
      cp -r ${webkitgtk_4_1}/lib/webkit2gtk-4.1/* $out/lib/x86_64-linux-gnu/webkit2gtk-4.1
    '';

  mesa-gbm =
    pkgs:
    pkgs.runCommand "mesa-gbm" { } ''
      mkdir -p $out/lib/x86_64-linux-gnu/gbm
      cp -r ${mesa}/lib/gbm/* $out/lib/x86_64-linux-gnu/gbm/
    '';

  linux = appimageTools.wrapType2 {
    inherit version src meta;
    pname = "mouseless";
    extraPkgs = pkgs: [
      (webkit-debian pkgs)
      (mesa-gbm pkgs)
    ];
    passthru = {
      updateScript = ./update.sh;
    };
  };

  darwin = stdenv.mkDerivation {
    pname = "mouseless";
    inherit version src meta;

    nativeBuildInputs = [
      undmg
      makeWrapper
    ];

    sourceRoot = ".";

    dontFixup = true;

    unpackCmd = ''
      echo "Creating temp directory"
      mnt=$(TMPDIR=/tmp mktemp -d -t nix-XXXXXXXXXX)
      function finish {
        echo "Ejecting temp directory"
        /usr/bin/hdiutil detach $mnt -force
        rm -rf $mnt
      }

      # Detach volume when receiving SIG 0
      trap finish EXIT

      # Mount DMG file
      echo "Mounting DMG file into \"$mnt\""
      /usr/bin/hdiutil attach -nobrowse -mountpoint $mnt $curSrc

      # Copy content to local dir for later use
      echo 'Copying extracted content into "sourceRoot"'
      cp -a "$mnt/${appName}" $PWD/
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/{Applications,bin}
      cp -r *.app $out/Applications/
      makeWrapper $out/Applications/Mouseless.app/Contents/MacOS/mouseless $out/bin/mouseless
      runHook postInstall
    '';
  };

in
if stdenv.hostPlatform.isDarwin then darwin else linux
