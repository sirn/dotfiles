{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "terminal-use";
  version = "1.2.0";

  src = fetchFromGitHub {
    owner = "flipbit03";
    repo = "terminal-use";
    rev = "v${version}";
    hash = "sha256-wHu+L//x1NiXTxD2mas0niV/TbTezg4MC7wUWAfgxpY=";
  };

  # Upstream keeps `version = "0.0.0"` in Cargo.toml and patches it to the
  # release tag in CI (see .github/workflows/release.yml). Mirror that here so
  # `tu --version` reports the real version. Cargo.lock's root entry is left
  # untouched so it matches the vendored copy captured before postPatch.
  postPatch = ''
    substituteInPlace Cargo.toml \
      --replace-fail 'version = "0.0.0"' 'version = "${version}"'
  '';

  cargoHash = "sha256-KapRznQ67o8H0aIMGvCMojwF/qSZ3rSlx6SEKbi12ig=";

  # Tests rely on a live PTY/daemon which the Nix build sandbox does not
  # provide; upstream CI covers them separately.
  doCheck = false;

  meta = with lib; {
    description = "Headless virtual terminal for AI agents (`tu`)";
    homepage = "https://github.com/flipbit03/terminal-use";
    license = licenses.mit;
    mainProgram = "tu";
    platforms = platforms.unix;
  };
}
