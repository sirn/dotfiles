{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchurl,
  makeWrapper,
}:
let
  sources = lib.importJSON ./sources.json;

  platformMap = {
    aarch64-darwin = "darwin-arm64";
    x86_64-darwin = "darwin-x64";
    aarch64-linux = "linux-arm64";
    x86_64-linux = "linux-x64";
  };

  system = stdenvNoCC.hostPlatform.system;
  target = platformMap.${system} or (throw "impeccable: unsupported platform ${system}");

  engineBin = fetchurl {
    url = "https://github.com/pbakaus/impeccable/releases/download/engine-v${sources.engineVersion}/impeccable-${target}";
    hash = sources.engineHashes.${system};
  };
in

stdenvNoCC.mkDerivation {
  pname = "impeccable";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "pbakaus";
    repo = "impeccable";
    inherit (sources) rev;
    hash = sources.srcHash;
  };

  nativeBuildInputs = [ makeWrapper ];

  # Preserve the prebuilt Mach-O/static-PIE engine binary without stripping.
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp -r .agents/skills $out/
    chmod +x $out/skills/impeccable/scripts/impeccable

    install -Dm755 ${engineBin} $out/skills/impeccable/scripts/bin/${target}/impeccable

    makeWrapper $out/skills/impeccable/scripts/bin/${target}/impeccable $out/bin/impeccable \
      --set-default IMPECCABLE_SKILL_DIR "$out/skills/impeccable"

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "Frontend design engine CLI and agent skill";
    homepage = "https://github.com/pbakaus/impeccable";
    license = lib.licenses.asl20;
    mainProgram = "impeccable";
    platforms = builtins.attrNames platformMap;
  };
}
