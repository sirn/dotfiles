{ stdenv, gitAndTools, fetchFromGitHub }:

stdenv.lib.overrideDerivation gitAndTools.gitflow (oldAttrs: {
  src = fetchFromGitHub {
    owner = "sirn";
    repo = "gitflow-avh";
    rev = "develop";
    sha256 = "03lc4k0jkxz989vvgbnp68apj68v61fn8q5l3zsvx434s7v4lz4b";
  };
})