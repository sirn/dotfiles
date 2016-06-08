{ stdenv, rebar3, fetchFromGitHub }:

stdenv.lib.overrideDerivation rebar3 (oldAttrs: {
  patches = [];
})