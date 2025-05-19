{ lib, fetchFromGitHub, emacs }:

with emacs.pkgs; melpaBuild rec {
  pname = "aidermacs";
  version = "1.3";

  src = fetchFromGitHub {
    owner = "MatthewZMD";
    repo = "aidermacs";
    rev = "v${version}";
    sha256 = "sha256-f6D8ZjCjGl11kbB/VQERuE2Fc/+T4OfH2B7sPGoE4ZY=";
  };

  packageRequires = [ markdown-mode ];

  meta = with lib; {
    homepage = "https://github.com/MatthewZMD/aidermacs";
    description = "AI Pair Programming in Emacs with Aider";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    inherit (emacs.meta) platforms;
  };
}
