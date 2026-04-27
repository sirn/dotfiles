{
  lib,
  stdenv,
  fetchFromGitHub,
  bun,
  cacert,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "pi-hashline-edit";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "RimuruW";
    repo = "pi-hashline-edit";
    rev = "refs/tags/v${finalAttrs.version}";
    hash = "sha256-ylpq7+rXDk2+c0Lvd73D1rkJ6onHo+1QiCiEbFA8MKY=";
  };

  nativeBuildInputs = [
    bun
    cacert
  ];

  dontConfigure = true;
  dontBuild = true;
  dontPatchELF = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    # Install production dependencies only
    bun install --frozen-lockfile --production --no-cache

    # Remove Bun's .cache directory (contains broken symlinks to build dir)
    rm -rf node_modules/.cache

    mkdir -p $out
    cp -r index.ts package.json src prompts node_modules $out/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Hashline read/edit tool override for pi-coding-agent";
    homepage = "https://github.com/RimuruW/pi-hashline-edit";
    license = licenses.mit;
  };
})
