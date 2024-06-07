{ pkgs, fetchFromGitHub, ... }:

pkgs.looking-glass-client.overrideAttrs (orig: rec {
  version = "B6";

  src = fetchFromGitHub {
    owner = "gnif";
    repo = "LookingGlass";
    rev = version;
    sha256 = "sha256-6vYbNmNJBCoU23nVculac24tHqH7F4AZVftIjL93WJU=";
    fetchSubmodules = true;
  };

  patches = [ ];
})
