{
  lib,
  stdenv,
  fetchFromGitHub,
  replaceVars,
  swaybg,
  meson,
  ninja,
  pkg-config,
  wayland-scanner,
  scdoc,
  libGL,
  wayland,
  libxkbcommon,
  pcre2,
  json_c,
  libevdev,
  pango,
  cairo,
  libinput,
  gdk-pixbuf,
  librsvg,
  wlroots,
  wayland-protocols,
  libdrm,
  nixosTests,
  # Used by the NixOS module:
  isNixOS ? false,
  enableXWayland ? true,
  xorg,
  systemdSupport ? lib.meta.availableOn stdenv.hostPlatform systemd,
  systemd,
  trayEnabled ? systemdSupport,

  # Path to nixpkgs source for referencing patches
  nixpkgsPath,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "sway-unwrapped-git";
  version = "0-unstable-2026-03-27";

  inherit
    enableXWayland
    isNixOS
    systemdSupport
    trayEnabled
    ;

  src = fetchFromGitHub {
    owner = "agorgl";
    repo = "sway";
    rev = "44f747c921f88258916b7447d5c6cf9c9169375c";
    hash = "sha256-UzV7yyYKLbM64Si3jHl1NI3Gy6uyax8CG4ypH2oT6P0=";
  };

  patches = [
    (nixpkgsPath + "/pkgs/by-name/sw/sway-unwrapped/load-configuration-from-etc.patch")

    (replaceVars (nixpkgsPath + "/pkgs/by-name/sw/sway-unwrapped/fix-paths.patch") { inherit swaybg; })
  ]
  ++ lib.optionals (!finalAttrs.isNixOS) [
    (nixpkgsPath + "/pkgs/by-name/sw/sway-unwrapped/sway-config-no-nix-store-references.patch")
  ]
  ++ lib.optionals finalAttrs.isNixOS [
    (nixpkgsPath + "/pkgs/by-name/sw/sway-unwrapped/sway-config-nixos-paths.patch")
  ];

  strictDeps = true;
  depsBuildBuild = [ pkg-config ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    wayland-scanner
    scdoc
  ];

  buildInputs = [
    libGL
    wayland
    libxkbcommon
    pcre2
    json_c
    libevdev
    pango
    cairo
    libinput
    gdk-pixbuf
    librsvg
    wayland-protocols
    libdrm
    (wlroots.override { inherit (finalAttrs) enableXWayland; })
  ]
  ++ lib.optionals finalAttrs.enableXWayland [ xorg.xcbutilwm ];

  mesonFlags =
    let
      inherit (lib.strings) mesonEnable mesonOption;

      sd-bus-provider = if systemdSupport then "libsystemd" else "basu";
    in
    [
      (mesonOption "sd-bus-provider" sd-bus-provider)
      (mesonEnable "tray" finalAttrs.trayEnabled)
    ];

  meta = {
    description = "I3-compatible tiling Wayland compositor (agorgl's fractional-scene fork)";
    homepage = "https://github.com/swaywm/sway/pull/8715";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "sway";
  };
})
