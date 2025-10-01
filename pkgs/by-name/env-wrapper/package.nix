{ lib, pkgs, ... }:

let
  python3 = pkgs.python3.withPackages (p: [ p.python-dotenv ]);
in
pkgs.writeScriptBin "envWrapper" ''
  #!${pkgs.runtimeShell}
  exec ${lib.getExe python3} ${./envwrap.py} "$@"
''
