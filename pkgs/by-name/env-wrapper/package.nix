{ lib, python3, writeScriptBin, runtimeShell }:

let
  python = python3.withPackages (p: [ p.python-dotenv ]);
in
writeScriptBin "envWrapper" ''
  #!${runtimeShell}
  exec ${lib.getExe python} ${./envwrap.py} "$@"
''
