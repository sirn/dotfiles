{ fetchurl, lib, stdenv, undmg }:

let
  appName = "Mouseless.app";
  version = "1.0.0-preview.1";
in
stdenv.mkDerivation {
  pname = "mouseless";
  inherit version;

  src = fetchurl {
    url = "https://github.com/croian/mouseless/releases/download/v${version}/mouseless-installer_v${version}.dmg";
    hash = "sha256-0JAZMDW1c7c2Jyopvf8wr7NRvxENYSrOKZ44QNmmPko=";
  };

  nativeBuildInputs = [ undmg ];

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
    mkdir -p $out/Applications
    cp -r *.app $out/Applications/
    runHook postInstall
  '';

  meta = {
    license = lib.licenses.unfree;
    homepage = "https://mouseless.click";
    description = "Keyboard-driven mouse control for macOS";
    platforms = lib.platforms.darwin;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
