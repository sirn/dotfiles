{
  lib,
  libvirt,
  pkg-config,
  systemd,
  fetchFromGitHub,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "udev-forwarder";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "sirn";
    repo = "udev-forwarder";
    rev = "v${version}";
    hash = "sha256-hy7o1qOL970Dfhy4NDNYv1i4CnUiD0bbjkR9iSb/Jkc=";
  };

  cargoHash = "sha256-N+HEC4gQ6w2bRWwnmUUrOcnqDeMguw8hKUKA0HrWWBY=";

  buildInputs = [
    libvirt
    systemd
  ];

  nativeBuildInputs = [ pkg-config ];

  meta = with lib; {
    description = "A simple daemon that forward matching USB devices to libvirt VM";
    homepage = "https://github.com/sirn/udev-forwarder";
    license = licenses.bsd3;
    mainProgram = "udev-forwarder";
  };
}
