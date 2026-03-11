{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  unzip,
}:

let
  version = "1.2.24";

  platformMap = {
    x86_64-linux = {
      arch = "linux-x64";
      ext = "tar.gz";
      hash = "sha256-IGRO9rhZdfC0nD6hMcjUnN7oVEGbO4z7JEduAXh6hx4=";
    };
    aarch64-linux = {
      arch = "linux-arm64";
      ext = "tar.gz";
      hash = "sha256-WFB7mMKQL9gZudJjlZTuxPNuVwjxFLoCixw+h3u15H0=";
    };
    aarch64-darwin = {
      arch = "darwin-arm64";
      ext = "zip";
      hash = "sha256-hYRnFVlDwQMIFQR8vaL9UHTRupMJoA5d1FhaGbPEiW4=";
    };
    x86_64-darwin = {
      arch = "darwin-x64";
      ext = "zip";
      hash = "sha256-TeRmLSCog02Pk+RZZf9fq/vA9p8V60uzPwdC7O1U4gM=";
    };
  };

  platform =
    platformMap.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "opencode-bin";
  inherit version;

  src = fetchurl {
    url = "https://github.com/anomalyco/opencode/releases/download/v${version}/opencode-${platform.arch}.${platform.ext}";
    inherit (platform) hash;
  };

  nativeBuildInputs =
    lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [ unzip ];

  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];

  sourceRoot = ".";

  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp opencode $out/bin/opencode
    chmod +x $out/bin/opencode
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "OpenCode - open-source AI coding agent for the terminal";
    homepage = "https://github.com/anomalyco/opencode";
    license = lib.licenses.mit;
    mainProgram = "opencode";
    platforms = lib.attrNames platformMap;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
