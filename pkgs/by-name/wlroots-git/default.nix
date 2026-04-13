{
  lib,
  stdenv,
  fetchFromGitLab,
  meson,
  ninja,
  pkg-config,
  wayland-scanner,
  libGL,
  wayland,
  wayland-protocols,
  libinput,
  libxkbcommon,
  pixman,
  libcap,
  libgbm,
  xorg,
  hwdata,
  seatd,
  vulkan-loader,
  glslang,
  libliftoff,
  libdisplay-info,
  lcms2,

  enableXWayland ? true,
  xwayland ? null,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "wlroots-git";
  version = "0-unstable-2026-03-27";

  inherit enableXWayland;

  src = fetchFromGitLab {
    domain = "gitlab.freedesktop.org";
    owner = "agorgl";
    repo = "wlroots";
    rev = "3265b228f8361cd0ec4adc9e601832263675d669";
    hash = "sha256-MmCIs+ttcuhMR3FFLh27oqqWx6P8+MbUxAVtLTggfCs=";
  };

  outputs = [
    "out"
    "examples"
  ];

  postUnpack = ''
    rm -r $sourceRoot/subprojects
  '';

  strictDeps = true;
  depsBuildBuild = [ pkg-config ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    wayland-scanner
    glslang
    hwdata
  ];

  buildInputs = [
    libliftoff
    libdisplay-info
    libGL
    libcap
    libinput
    libxkbcommon
    libgbm
    pixman
    seatd
    vulkan-loader
    wayland
    xorg.libX11
    xorg.xcbutilerrors
    xorg.xcbutilimage
    xorg.xcbutilrenderutil
    xorg.xcbutilwm
    lcms2
    wayland-protocols
  ]
  ++ lib.optional finalAttrs.enableXWayland xwayland;

  mesonFlags = lib.optional (!finalAttrs.enableXWayland) "-Dxwayland=disabled";

  postFixup = ''
    mkdir -p $examples/bin
    cd ./examples
    for binary in $(find . -executable -type f -printf '%P\n' | grep -vE '\.so'); do
      cp "$binary" $examples/bin/wlroots-$binary
    done
  '';

  meta = {
    description = "Modular Wayland compositor library (agorgl's fractional-scene fork)";
    homepage = "https://gitlab.freedesktop.org/wlroots/wlroots/-/merge_requests/5071";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
})
