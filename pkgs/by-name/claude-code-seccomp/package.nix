{
  lib,
  stdenv,
  fetchurl,
}:

let
  version = "0.0.40";

  archMap = {
    x86_64-linux = "x64";
    aarch64-linux = "arm64";
  };

  arch =
    archMap.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "claude-code-seccomp";
  inherit version;

  src = fetchurl {
    url = "https://registry.npmjs.org/@anthropic-ai/sandbox-runtime/-/sandbox-runtime-${version}.tgz";
    hash = "sha256-s1U18yiX2EMV+MfJnXQQCyqtKzftT5ZWbGjnxMlXjaU=";
  };

  buildPhase = ''
    runHook preBuild
    gcc -O2 -o apply-seccomp vendor/seccomp-src/apply-seccomp.c
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 apply-seccomp $out/bin/apply-seccomp
    install -Dm644 vendor/seccomp/${arch}/unix-block.bpf \
      $out/share/claude-code-seccomp/unix-block.bpf
    runHook postInstall
  '';

  meta = {
    description = "Seccomp filter tools for Claude Code sandbox";
    homepage = "https://github.com/anthropics/claude-code";
    license = lib.licenses.unfree;
    platforms = lib.platforms.linux;
  };
}
