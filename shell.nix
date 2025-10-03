let
  inherit (import ./. { }) pkgs;
in
pkgs.mkShellNoCC { }