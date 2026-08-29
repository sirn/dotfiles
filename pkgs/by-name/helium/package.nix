{
  lib,
  stdenv,
  coreutils,
  fetchurl,
  zlib,
  libx11,
  libxext,
  libxcb,
  libxau,
  libxdmcp,
  libxkbcommon,
  libxshmfence,
  libxfixes,
  libxi,
  libxcursor,
  libxscrnsaver,
  libxcomposite,
  libxdamage,
  libxtst,
  libxrandr,
  libxrender,
  alsa-lib,
  dbus,
  cups,
  expat,
  libdrm,
  libgbm,
  libGL,
  libGLU,
  libglvnd,
  vulkan-loader,
  systemd,
  libva,
  freetype,
  fontconfig,
  libxml2,
  glib,
  pango,
  cairo,
  gdk-pixbuf,
  atk,
  at-spi2-atk,
  at-spi2-core,
  nss,
  nspr,
  libexif,
  ffmpeg,
  pipewire,
  libpulseaudio,
  libkrb5,
  qt6,
  gtk3,
  gtk4,
  adwaita-icon-theme,
  gsettings-desktop-schemas,
  patchelf,
  makeWrapper,
  addDriverRunpath,
  commandLineArgs ? "",
}:

let
  # Chromium-based browsers ship as prebuilt ELF binaries wrapped by a
  # Debian package. We unpack the .deb, patchelf the binaries/libs to find
  # their shared libraries through the Nix store, then re-wrap the launcher.
  #
  # Modeled after nixpkgs' vivaldi package.

  sources = lib.importJSON ./sources.json;

  suffix =
    {
      aarch64-linux = "arm64";
      x86_64-linux = "amd64";
    }
    .${stdenv.hostPlatform.system} or (throw "Unsupported system: ${stdenv.hostPlatform.system}");

  buildInputs = [
    stdenv.cc.cc
    stdenv.cc.libc
    zlib
    libx11
    libxext
    libxcb
    libxau
    libxdmcp
    libxkbcommon
    libxshmfence
    libxfixes
    libxi
    libxcursor
    libxscrnsaver
    libxcomposite
    libxdamage
    libxtst
    libxrandr
    libxrender
    atk
    at-spi2-atk
    at-spi2-core
    alsa-lib
    dbus
    cups
    expat
    glib
    nss
    nspr
    libGL
    libGLU
    libglvnd
    libxml2
    pango
    cairo
    gdk-pixbuf
    freetype
    fontconfig
    libdrm
    libgbm
    vulkan-loader
    systemd
    libva
    libexif
    ffmpeg
    pipewire
    libpulseaudio
    libkrb5
    qt6.qtbase
    qt6.qtwayland
    # Needed for GSETTINGS_SCHEMAS_PATH so Chromium can read
    # org.gnome.desktop.interface font-name for its UI font.
    gsettings-desktop-schemas
    glib
    gtk3
    gtk4
    # Needed for XDG_ICON_DIRS
    adwaita-icon-theme
  ];

  libPath =
    lib.makeLibraryPath buildInputs
    + lib.optionalString stdenv.hostPlatform.is64bit (
      ":" + lib.makeSearchPathOutput "lib" "lib64" buildInputs
    )
    + ":$out/opt/helium/lib";
in
stdenv.mkDerivation rec {
  pname = "helium";
  version = "0.16.2.1";

  src = fetchurl {
    url = "https://github.com/imputnet/helium-linux/releases/download/${version}/helium-bin_${version}-1_${suffix}.deb";
    hash =
      sources.${stdenv.hostPlatform.system} or (throw "No source hash for ${stdenv.hostPlatform.system}");
  };

  unpackPhase = ''
    runHook preUnpack
    ar vx $src
    tar -xvf data.tar.xz
    runHook postUnpack
  '';

  nativeBuildInputs = [
    patchelf
    makeWrapper
    qt6.wrapQtAppsHook
  ];

  dontWrapQtApps = true;

  inherit buildInputs;

  buildPhase = ''
    runHook preBuild
    echo "Patching Helium binaries"
    for f in helium helium_crashpad_handler chromedriver; do
      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath "${libPath}" \
        opt/helium/$f
    done

    for f in libEGL.so libGLESv2.so libqt5_shim.so libqt6_shim.so libvk_swiftshader.so; do
      patchelf --set-rpath "${libPath}" opt/helium/$f
    done

    echo "Finished patching Helium binaries"
    runHook postBuild
  '';

  dontPatchELF = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r opt "$out"
    mkdir "$out/bin"
    ln -s "$out/opt/helium/helium-wrapper" "$out/bin/helium"
    mkdir -p "$out/share"
    cp -r usr/share/{applications,icons,metainfo} "$out"/share

    substituteInPlace "$out"/share/applications/helium.desktop \
      --replace-fail "Exec=helium" "Exec=$out/bin/helium"

    # Replace bundled vulkan-loader with the NixOS-patched one so Vulkan ICD
    # discovery works.
    rm -f $out/opt/helium/libvulkan.so.1
    ln -s "${lib.getLib vulkan-loader}/lib/libvulkan.so.1" $out/opt/helium/libvulkan.so.1

    wrapProgram "$out/bin/helium" \
      --add-flags ${lib.escapeShellArg commandLineArgs} \
      --prefix XDG_DATA_DIRS : "${addDriverRunpath.driverLink}/share:${glib}/share/gsettings-schemas/${glib.name}:$GSETTINGS_SCHEMAS_PATH:$XDG_ICON_DIRS" \
      --prefix LD_LIBRARY_PATH : ${libPath} \
      --prefix PATH : ${coreutils}/bin \
      ''${qtWrapperArgs[@]}
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Private, fast, and honest web browser based on Chromium";
    homepage = "https://github.com/imputnet/helium-linux";
    license = lib.licenses.gpl3Plus;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    mainProgram = "helium";
    # The upstream release only ships Linux (Debian/AppImage/tar) binaries.
    # macOS support is tracked upstream; the package is structured so a
    # darwin branch can be added by extending `sources` and `suffix` once
    # assets are published.
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
