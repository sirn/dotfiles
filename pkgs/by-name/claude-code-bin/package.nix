{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  bubblewrap ? null,
  socat ? null,
  procps ? null,
}:

let
  version = "2.1.72";

  baseUrl = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases";

  platformMap = {
    x86_64-linux = {
      platform = "linux-x64";
      hash = "sha256-tVM45/u4v30mi5G6s7KHU2Idq5Y3scypQ2afRJDth40=";
    };
    aarch64-linux = {
      platform = "linux-arm64";
      hash = "sha256-nwwQy50iLq9OxIcEA6FWGxMp3GaTA2Ezaah3/QWhFwg=";
    };
    aarch64-darwin = {
      platform = "darwin-arm64";
      hash = "sha256-xYT1E2LVYmlbxxdQ0cIZb5oODjb9BD4rxoPM/Jo6s9c=";
    };
    x86_64-darwin = {
      platform = "darwin-x64";
      hash = "sha256-JLn6GD5CJmQPCiFY53cCsN2GDZIFsb7H5pVgmjCciYY=";
    };
  };

  platform =
    platformMap.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "claude-code-bin";
  inherit version;

  src = fetchurl {
    url = "${baseUrl}/${version}/${platform.platform}/claude";
    inherit (platform) hash;
  };

  nativeBuildInputs =
    [ makeWrapper ] ++ lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];

  dontUnpack = true;
  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp $src $out/bin/.claude-wrapped
    chmod +x $out/bin/.claude-wrapped
    makeWrapper $out/bin/.claude-wrapped $out/bin/claude \
      --prefix PATH : "${lib.makeBinPath (lib.optionals stdenv.hostPlatform.isLinux [ bubblewrap socat procps ])}"
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Claude Code - an agentic coding tool that lives in your terminal";
    homepage = "https://github.com/anthropics/claude-code";
    license = lib.licenses.unfree;
    mainProgram = "claude";
    platforms = lib.attrNames platformMap;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
