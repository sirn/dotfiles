{ lib, config, pkgs, ... }:

let
  package = pkgs.local.octofriend;
in
{
  home.packages = [
    (pkgs.writeScriptBin "octo" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i ~/.config/llm-agent/env \
        -- "${package}/bin/octo" "$@"
    '')
  ];
}
