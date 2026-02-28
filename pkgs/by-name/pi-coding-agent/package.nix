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
  version = "0.55.3";

  platformMap = {
    x86_64-linux = {
      arch = "linux-x64";
      hash = "sha256:sha256-Az8qDfnLk6y667/1DbLuf64nSWcrPP5mpOqgY7yOMTY=";
    };
    aarch64-linux = {
      arch = "linux-arm64";
      hash = "sha256:1cyza1x0z5x8gvqyhwpgv78c0l4258vx3b9cy6lqrdz6pgl9dliw";
    };
    aarch64-darwin = {
      arch = "darwin-arm64";
      hash = "sha256:1k31r52n69pml6jvl23h8j3znnqn0zkg4bbcrd0n6zkli950vfgv";
    };
    x86_64-darwin = {
      arch = "darwin-x64";
      hash = "sha256:1ad58497l0hrb2s7qsdp0fgrb945xsanqfzlxxxkqxg7064k0yfj";
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
