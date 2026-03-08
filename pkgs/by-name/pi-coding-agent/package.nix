{ lib
, stdenv
, fetchurl
, autoPatchelfHook
, makeWrapper
, fd
, ripgrep
,
}:

let
  version = "0.57.1";

  platformMap = {
    x86_64-linux = {
      arch = "linux-x64";
      hash = "sha256-ghQwKFWi+oPRW0cGvI6L6Rommqy0lhTBSqc/0sKqZac=";
    };
    aarch64-linux = {
      arch = "linux-arm64";
      hash = "sha256-GrX78MvEEKaM6MOAEGttfW4v6ryg3KJ7jmi3dw6h9K0=";
    };
    aarch64-darwin = {
      arch = "darwin-arm64";
      hash = "sha256-T8cLJKI+ODscNW6jdiDhGJ1ohCDfNrSMSbR1eJ9eD44=";
    };
    x86_64-darwin = {
      arch = "darwin-x64";
      hash = "sha256-vGdzPOmldi00rmCP62Va53Q08fU1qgimXG3kBFt37HM=";
    };
  };

  platform =
    platformMap.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation rec {
  pname = "pi-coding-agent";
  inherit version;

  src = fetchurl {
    url = "https://github.com/badlogic/pi-mono/releases/download/v${version}/pi-${platform.arch}.tar.gz";
    inherit (platform) hash;
  };

  nativeBuildInputs = [
    makeWrapper
  ]
  ++ lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];

  buildInputs = [ ];

  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/pi-coding-agent

    # Install the binary and assets in share/ so Bun can find themes
    cp pi $out/share/pi-coding-agent/pi
    chmod +x $out/share/pi-coding-agent/pi
    cp -r theme export-html docs examples photon_rs_bg.wasm package.json README.md CHANGELOG.md $out/share/pi-coding-agent/

    # Wrapper script: binary runs from share/ where themes live
    makeWrapper $out/share/pi-coding-agent/pi $out/bin/pi \
      --set PI_PACKAGE_DIR "$out/share/pi-coding-agent" \
      --set PI_SKIP_VERSION_CHECK "1" \
      --prefix PATH : "${
        lib.makeBinPath [
          fd
          ripgrep
        ]
      }"

    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Minimal terminal coding agent with read, bash, edit, write tools";
    homepage = "https://github.com/badlogic/pi-mono";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "pi";
    platforms = lib.attrNames platformMap;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
