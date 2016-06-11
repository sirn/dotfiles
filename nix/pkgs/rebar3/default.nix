{ stdenv, rebar3, fetchFromGitHub }:

rebar3.overrideDerivation (oldAttrs: {
  patches = [];
})