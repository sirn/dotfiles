{
  lib,
  libvirt,
  pkg-config,
  systemd,
  fetchFromSourcehut,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "udev-forwarder";
  version = "0.1.1";

  src = fetchFromSourcehut {
    owner = "~sirn";
    repo = "udev-forwarder";
    rev = "v${version}";
    hash = "sha256-ZOLVTLKUFzmQ41bGBp7mm3u4cm+tkz9648kFneXEZ9c=";
  };

  cargoHash = "sha256-ILrkx6JaI69tBRLZNA5X/blO3KKqJVunLk1OWiUINHw=";

  buildInputs = [
    libvirt
    systemd
  ];

  nativeBuildInputs = [ pkg-config ];

  meta = with lib; {
    description = "A simple daemon that forward matching USB devices to libvirt VM";
    homepage = "https://git.sr.ht/~sirn/udev-forwarder";
    license = licenses.bsd3;
    mainProgram = "udev-forwarder";
  };
}
