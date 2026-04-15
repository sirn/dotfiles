{ pkgs, ... }:

let
  # pkgs.jaq (2.x) lacks --from; use unstable (3.x) for multi-format support.
  # Revisit for 26.05 when nixos-26.05 ships jaq 3.x.
  jaq-with-jq-alias = pkgs.symlinkJoin {
    name = "jaq-with-jq-alias";
    paths = [ pkgs.unstable.jaq ];
    postBuild = ''
      ln -s jaq $out/bin/jq
    '';
  };
in
{
  home.packages = [ jaq-with-jq-alias ];
}
