{ lib, pkgs, buildGoModule, fetchgit, stdenv, lsof }:

buildGoModule rec {
  pname = "tincan";
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/tincan";
    rev = "refs/tags/v${version}";
    hash = "sha256-Y9mgqbDv5jv2gRIIUlQXit7r3rqMNbFH5jhrQiSCKLE=";
  };

  vendorHash = "sha256-A7ePGlDtqds0T4uDGXgCX/Pv1q0VRMQczeGWtb/b3ps=";

  nativeCheckInputs = lib.optionals stdenv.hostPlatform.isDarwin [ lsof ];

  meta = with lib; {
    description = "Automatic SSH port forwarding";
    homepage = "https://git.sr.ht/~sirn/tincan";
    license = licenses.mit;
    mainProgram = "tincan";
  };
}
