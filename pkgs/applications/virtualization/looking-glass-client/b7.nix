{ pkgs, fetchFromGitHub, ... }:

pkgs.looking-glass-client.overrideAttrs (orig: rec {
  version = "B7-rc1";

  src = fetchFromGitHub {
    owner = "gnif";
    repo = "LookingGlass";
    rev = version;
    sha256 = "sha256-ne1Q+67+P8RHcTsqdiSSwkFf0g3pSNT91WN/lsSzssU=";
    fetchSubmodules = true;
  };

  patches = [ ];
})
