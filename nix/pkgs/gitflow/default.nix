{ stdenv, gitAndTools, fetchFromGitHub }:

gitAndTools.gitflow.overrideDerivation (oldAttrs: {
  src = fetchFromGitHub {
    owner = "petervanderdoes";
    repo = "gitflow-avh";
    rev = "b261d1c61026822dedca02ede100e5df27e1d5bf";
    sha256 = "1aqay58z16cnrbhqflrgkkviphavbi18iiqks1hgjmjymkxkrqnd";
  };
})