{
  lib,
  stdenv,
  fetchFromGitHub,
  bash,
  python3,
  gawk,
  ...
}:

stdenv.mkDerivation rec {
  pname = "node-exporter-textfile-collector-scripts";
  version = "20240214";
  commit = "ef8c077";

  src = fetchFromGitHub {
    owner = "prometheus-community";
    repo = "node-exporter-textfile-collector-scripts";
    rev = "${commit}";
    hash = "sha256-QvEeezCIe+VDGZJqs9J+QQds3+inU+CWLXMDo9D7nQk=";
  };

  nativeBuildInputs = [
    bash
    gawk
    python3
  ];

  buildPhase = ''
    patchShebangs .
  '';

  installPhase = ''
    mkdir -p $out/libexec/node-exporter-textfile-collector-scripts
    cp {*.py,*.sh,ipmitool,lvm-prom-collector,mellanox_hca_temp,multipathd_info} $out/libexec/node-exporter-textfile-collector-scripts
    chmod +x $out/libexec/node-exporter-textfile-collector-scripts/*
  '';
}
