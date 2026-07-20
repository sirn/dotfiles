{
  lib,
  stdenv,
  rustPlatform,
  fetchgit,
  pkg-config,
  cmake,
  makeWrapper,
  addDriverRunpath,
  llvmPackages,
  writeShellScript,

  # Linux only
  alsa-lib,
  vulkan-headers,
  vulkan-loader,
  shaderc,
  wayland,
  fontconfig,
}:

let
  # In-bundle launcher so the service can source an env file (API keys) while
  # keeping the process tree attributed to Handsfree.app for TCC. Built as a
  # separate derivation and copied into Contents/MacOS in postInstall so
  # codesign --deep in postFixup covers it.
  darwinLauncher = writeShellScript "handsfree-launcher" ''
    f="$1"
    if [ -n "$f" ] && [ -f "$f" ]; then
      set -a
      . "$f"
      set +a
    fi
    exec "${"\${0%/*}"}/handsfree"
  '';
in
rustPlatform.buildRustPackage rec {
  pname = "handsfree";
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/handsfree";
    rev = "refs/tags/v${version}";
    hash = "sha256-itjqY1+hVnzkc5QxcAiw/xgeq5OAmWsjj9Y1N7qrP5A=";
  };

  cargoHash = "sha256-pTO798AyrXAxyzIkhCEiTe73zHZIV2xBBkzW7ORK7qw=";

  nativeBuildInputs = [
    pkg-config
    cmake
    makeWrapper
    llvmPackages.libclang
  ]
  ++ lib.optionals stdenv.hostPlatform.isLinux [ shaderc ];

  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
    alsa-lib
    vulkan-headers
    vulkan-loader
    wayland
    fontconfig
  ];

  # bindgen (whisper-rs-sys) needs libclang at build time.
  LIBCLANG_PATH = "${lib.getLib llvmPackages.libclang}/lib";

  cargoBuildFlags = [
    "-p"
    "handsfree"
  ];

  doCheck = false;

  # The Apple FoundationModels / SFSpeech helpers are built best-effort by the
  # crates' build.rs scripts via the system Xcode toolchain (skipped without an
  # SDK). The runtime resolves them next to the executable first, then falls
  # back to the build-time OUT_DIR path (which is gone post-install), so copy
  # whatever was produced while the cargo target dir still exists. Cargo emits
  # build-script outputs under target/<triple>/release/build/ when --target is
  # set; match both layouts so the glob survives a host-only build too.
  postBuild = lib.optionalString stdenv.hostPlatform.isDarwin ''
    mkdir -p $NIX_BUILD_TOP/hf-helpers
    for h in target/release/build/handsfree-llm-*/out/hf-apple-clean \
             target/*/release/build/handsfree-llm-*/out/hf-apple-clean \
             target/release/build/handsfree-transcribe-*/out/hf-apple-transcribe \
             target/*/release/build/handsfree-transcribe-*/out/hf-apple-transcribe; do
      [ -f "$h" ] && cp "$h" $NIX_BUILD_TOP/hf-helpers/ || true
    done
  '';

  postInstall =
    lib.optionalString stdenv.hostPlatform.isDarwin ''
      app=$out/Applications/Handsfree.app
      mkdir -p $app/Contents/MacOS $app/Contents/Resources
      cp $out/bin/handsfree $app/Contents/MacOS/
      cp -f $NIX_BUILD_TOP/hf-helpers/hf-apple-* $app/Contents/MacOS/ 2>/dev/null || true
      cp ${darwinLauncher} $app/Contents/MacOS/handsfree-launcher
      chmod +x $app/Contents/MacOS/handsfree-launcher
      cp contrib/macos/Info.plist $app/Contents/
      substituteInPlace $app/Contents/Info.plist \
        --replace-fail "@VERSION@" "${version}"
    ''
    + lib.optionalString stdenv.hostPlatform.isLinux ''
      wrapProgram $out/bin/handsfree \
        --prefix LD_LIBRARY_PATH : ${
          lib.makeLibraryPath [
            vulkan-loader
            alsa-lib
            wayland
            fontconfig
            addDriverRunpath.driverLink
          ]
        } \
        --prefix PATH : ${lib.makeBinPath [ fontconfig.bin ]}
    '';

  # Seal the bundle ad-hoc AFTER fixupPhase (which strips/rewrites signatures)
  # so TCC binds Info.plist (CFBundleIdentifier dev.sirn.handsfree) to the
  # responsible process. Without this the binary is only linker-signed and the
  # plist is not bound, which makes Accessibility/Microphone grants unreliable.
  postFixup = lib.optionalString stdenv.hostPlatform.isDarwin ''
    /usr/bin/codesign --deep --force --sign - $out/Applications/Handsfree.app
  '';

  meta = with lib; {
    description = "Minimal local speech-to-text dictation app for macOS and Wayland";
    homepage = "https://git.sr.ht/~sirn/handsfree";
    license = licenses.mit;
    mainProgram = "handsfree";
    platforms = platforms.linux ++ platforms.darwin;
  };
}
