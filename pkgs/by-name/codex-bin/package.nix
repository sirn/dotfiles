{
  lib,
  stdenv,
  fetchurl,
}:

let
  version = "0.114.0";

  platformMap = {
    x86_64-linux = {
      target = "x86_64-unknown-linux-musl";
      hash = "sha256-kinejFHI7zBWW7UHyXou3ASoCzjkmkNj8zf+Bb7fNOs=";
    };
    aarch64-linux = {
      target = "aarch64-unknown-linux-musl";
      hash = "sha256-fTBzVoEHfBO28NpuiCo6r5ZY3yDRVfXZkiL7ex0pAJk=";
    };
    aarch64-darwin = {
      target = "aarch64-apple-darwin";
      hash = "sha256-yY61UGlfmersJ9+ZcaG3aoOssV61VSI4P6MbBJcpfFQ=";
    };
    x86_64-darwin = {
      target = "x86_64-apple-darwin";
      hash = "sha256-sMfOn242k+zBULwMuVtKF2bXuEl0cJDlB33o1NLKyXQ=";
    };
  };

  platform =
    platformMap.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "codex-bin";
  inherit version;

  src = fetchurl {
    url = "https://github.com/openai/codex/releases/download/rust-v${version}/codex-${platform.target}.tar.gz";
    inherit (platform) hash;
  };

  sourceRoot = ".";

  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp codex-${platform.target} $out/bin/codex
    chmod +x $out/bin/codex
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "OpenAI Codex CLI - lightweight coding agent for the terminal";
    homepage = "https://github.com/openai/codex";
    license = lib.licenses.asl20;
    mainProgram = "codex";
    platforms = lib.attrNames platformMap;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
