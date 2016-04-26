{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
in
rec {
  aria2 = import ./pkgs/aria2 {
    inherit (pkgs) stdenv fetchurl pkgconfig autoreconfHook cacert openssl c-ares libxml2 sqlite zlib libssh2;
  };

  trash = import ./pkgs/trash {
    inherit (pkgs) stdenv lib fetchurl;
    inherit (pkgs.darwin.apple_sdk.frameworks) AppKit Cocoa ScriptingBridge;
  };
}
