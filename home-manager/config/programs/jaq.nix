{ pkgs, ... }:

let
  jaq-with-jq-alias = pkgs.symlinkJoin {
    name = "jaq-with-jq-alias";
    paths = [ pkgs.jaq ];
    postBuild = ''
      ln -s jaq $out/bin/jq
    '';
  };
in
{
  home.packages = [ jaq-with-jq-alias ];
}
