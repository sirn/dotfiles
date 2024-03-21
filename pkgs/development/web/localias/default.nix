{ buildGo120Module, fetchFromGitHub, ... }:

buildGo120Module rec {
  pname = "localias";
  version = "2.0.0";
  commit = "6fd5535";

  src = fetchFromGitHub {
    owner = "peterldowns";
    repo = "localias";
    rev = "${version}+commit.${commit}";
    hash = "sha256-2yvfsoGOWAcOietKYqvFbL9IkeVSI6zo71I742q68Ac=";
  };

  vendorHash = "sha256-L81PJ1MpXFfcZ/BPYaYlr2rS549i6Lle9l9IRIhh2iE=";

  buildInputs = [ ];
  ldflags = [
    "-X github.com/peterldowns/localias/cmd/localias/shared.Version=${version}"
    "-X github.com/peterldowns/localias/cmd/localias/shared.Commit=${commit}"
  ];

  modRoot = "./.";
  subPackages = [ "cmd/localias" ];
  doCheck = false;
}
