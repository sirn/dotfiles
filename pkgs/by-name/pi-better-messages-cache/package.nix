{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage {
  pname = "pi-better-messages-cache";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "mcowger";
    repo = "pi-better-messages-cache";
    rev = "5508d58a75acad963e4de3f92148e299c2afdabc";
    hash = "sha256-HuqrHu5UbGUkMFvVd2Ijg9xxhvvsoMoLxbCLOTiDhd8=";
  };

  npmDepsHash = "sha256-zoDsqXYAWkXcrvlh5G+y5FO2GvJyQRGzpPnb2doPZv8=";

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp index.ts $out/
    cp -r node_modules $out/
    runHook postInstall
  '';

  meta = {
    description = "Pi extension implementing dual cache-breakpoint strategy for Anthropic models";
    homepage = "https://github.com/mcowger/pi-better-messages-cache";
    license = lib.licenses.mit;
  };
}
