{
  lib,
  stdenv,
  fetchurl,
  pkg-config,
  meson,
  ninja,
  wayland-scanner,
  python3,
  wayland,
  testers,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "wayland-protocols";
  version = "1.47";

  doCheck =
    stdenv.hostPlatform == stdenv.buildPlatform
    && stdenv.hostPlatform.linker == "bfd"
    && !(stdenv.hostPlatform.isPower64 && stdenv.hostPlatform.isBigEndian)
    && lib.meta.availableOn stdenv.hostPlatform wayland;

  src = fetchurl {
    url = "https://gitlab.freedesktop.org/wayland/${finalAttrs.pname}/-/releases/${finalAttrs.version}/downloads/${finalAttrs.pname}-${finalAttrs.version}.tar.xz";
    hash = "sha256-X9Q0m8vJurmkb4z3fR9DQpanoFLIdECglPY/z2KljiA=";
  };

  postPatch = lib.optionalString finalAttrs.finalPackage.doCheck ''
    patchShebangs tests/
  '';

  depsBuildBuild = [ pkg-config ];
  nativeBuildInputs = [
    meson
    ninja
    wayland-scanner
  ];
  nativeCheckInputs = [
    python3
    wayland
  ];
  checkInputs = [ wayland ];
  strictDeps = true;

  mesonFlags = [ "-Dtests=${lib.boolToString finalAttrs.finalPackage.doCheck}" ];

  meta = {
    description = "Wayland protocol extensions";
    homepage = "https://gitlab.freedesktop.org/wayland/wayland-protocols";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
    pkgConfigModules = [ "wayland-protocols" ];
  };

  passthru.version = finalAttrs.version;
  passthru.tests.pkg-config = testers.hasPkgConfigModules { package = finalAttrs.finalPackage; };
})
