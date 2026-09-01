{
  lib,
  buildGoModule,
  fetchgit,
  stdenv,
  lsof,
}:

buildGoModule rec {
  pname = "tincan";
  version = "0.2.1";

  src = fetchgit {
    url = "https://github.com/sirn/tincan";
    rev = "refs/tags/v${version}";
    hash = "sha256-l2lUmwsR/5wfabM7bC7twpNXd+/uIah4v9+BlxJ+U7w=";
  };

  vendorHash = "sha256-A7ePGlDtqds0T4uDGXgCX/Pv1q0VRMQczeGWtb/b3ps=";

  nativeCheckInputs = lib.optionals stdenv.hostPlatform.isDarwin [ lsof ];

  meta = with lib; {
    description = "Automatic SSH port forwarding";
    homepage = "https://github.com/sirn/tincan";
    license = licenses.mit;
    mainProgram = "tincan";
  };
}
