{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  fd,
  ripgrep,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version systems;

  system =
    systems.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "pi-coding-agent";
  inherit version;

  src = fetchurl { inherit (system) url hash; };

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
    platforms = lib.attrNames systems;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
