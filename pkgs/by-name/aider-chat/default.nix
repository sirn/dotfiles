{ lib
, pkgs
, fetchFromGitHub
}:

let
  version = "0.84.0";
in
pkgs.aider-chat-full.overrideAttrs (oldAttrs: {
  inherit version;

  src = fetchFromGitHub {
    owner = "Aider-AI";
    repo = "aider";
    rev = "v${version}";
    hash = "sha256-TOlqwJM9wIAURSimuh9mysYDwgH9AfFev8jY9elLNk8=";
  };

  meta = with lib; {
    description = "AI pair programming in your terminal";
    homepage = "https://github.com/Aider-AI/aider";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
})
