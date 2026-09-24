{
  lib,
  rustdesk-flutter,
  flutter329,
  fetchFromGitHub,
  rustPlatform,
  libdrmtap,
  libglvnd,
  addDriverRunpath,
  systemd,
  procps,
  coreutils,
  findutils,
  util-linux,
  getent,
  gawk,
  gnugrep,
  gnused,
  which,
  xdg-utils,
  xdg-user-dirs,
  testers,
}:

rustdesk-flutter.override {
  inherit flutter329;
  buildFlutterApplication =
    previousAttrs:
    flutter329.buildFlutterApplication (
      finalAttrs:
      previousAttrs
      // {
        version = "1.5.0";
        __structuredAttrs = true;

        src = fetchFromGitHub {
          owner = "rustdesk";
          repo = "rustdesk";
          tag = finalAttrs.version;
          fetchSubmodules = true;
          hash = "sha256-1xa7X+swBIb8Lz3c6m8SeNZAiJWNCUpw+UbdSsMkeSk=";
        };

        sourceRoot = "${finalAttrs.src.name}/flutter";

        # Resolved with Flutter 3.29.3, including extended_text 15 for its selection API.
        pubspecLock = lib.importJSON ./pubspec.lock.json;
        gitHashes = lib.importJSON ./git-hashes.json;
        # Nix ignores the Dart builder's passAsFile with structured attributes.
        pubspecLockFilePath = ./pubspec.lock.json;

        cargoDeps = rustPlatform.fetchCargoVendor {
          inherit (finalAttrs) pname version src;
          hash = "sha256-Ym4USlB1NJO0bm0cr2l/yZhoUBfxtd2uTkkjrR7iD3o=";
        };

        # Upstream ships these features off by default; enable them here.
        cargoBuildFeatures = previousAttrs.cargoBuildFeatures ++ [
          "drm"
          "drm-wake"
        ];

        patches = previousAttrs.patches ++ [ ./reenter-wrapper.patch ];

        postPatch = ''
          # Root must load the capture library from a trusted absolute path.
          substituteInPlace libs/scrap/src/common/drmtap_dl.rs \
            --replace-fail '/usr/lib/rustdesk/libdrmtap.so.0' '${lib.getLib libdrmtap}/lib/libdrmtap.so.0'
          substituteInPlace src/platform/linux.rs \
            --replace-fail '@rustdesk-wrapper@' '${placeholder "out"}/bin/rustdesk'
          substituteInPlace flutter/pubspec.yaml \
            --replace-fail 'extended_text: 14.0.0' 'extended_text: 15.0.2'
        ''
        + previousAttrs.postPatch;

        # Prefer the host's setuid wrappers on NixOS; elsewhere sudo/su are found
        # through the inherited PATH. Store sudo/su binaries are not setuid.
        extraWrapProgramArgs = ''
          --prefix LD_LIBRARY_PATH : ${addDriverRunpath.driverLink}/lib \
          --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ libglvnd ]} \
          --prefix PATH : /run/wrappers/bin:${
            lib.makeBinPath [
              systemd
              procps
              coreutils
              findutils
              util-linux
              getent
              gawk
              gnugrep
              gnused
              which
              xdg-utils
              xdg-user-dirs
            ]
          }
        '';

        doInstallCheck = true;
        installCheckPhase = ''
          runHook preInstallCheck

          rustLibrary="$out/app/$pname/lib/librustdesk.so"
          test -x "$out/bin/rustdesk"
          grep -aFq '${lib.getLib libdrmtap}/lib/libdrmtap.so.0' "$rustLibrary"
          grep -aFq 'enable-drm-display-wake' "$rustLibrary"
          grep -aFq "$out/bin/rustdesk" "$rustLibrary"
          if grep -aFq '/usr/lib/rustdesk/libdrmtap.so.0' "$rustLibrary"; then
            echo 'Unpatched DRM loader path in RustDesk' >&2
            exit 1
          fi

          runHook postInstallCheck
        '';

        passthru = (previousAttrs.passthru or { }) // {
          tests.version = testers.testVersion {
            package = finalAttrs.finalPackage;
            # Upstream identifies the development version as 1.5.0.
            version = "1.5.0";
          };
        };

        meta = previousAttrs.meta // {
          description = "Remote desktop client with unattended Wayland support";
          changelog = "https://github.com/rustdesk/rustdesk/compare/1.4.9...${finalAttrs.version}";
        };
      }
    );
}
