{
  config,
  lib,
  pkgs,
  ...
}:

let
  # nom only renders nix's internal-json log, and a function is needed over an
  # alias so arguments stay in front of the pipe. A CLI is wrapped only when it
  # is on PATH, and `command` calls the real CLI instead of recursing into the
  # wrapper.
  clis = [
    "home-manager"
    "nixos-rebuild"
  ];

  posixWrappers = lib.concatMapStrings (cli: ''
    if command -v ${cli} >/dev/null 2>&1; then
      ${cli}() {
        command ${cli} --log-format internal-json -v "$@" |& nom --json
      }
    fi
  '') clis;
in
{
  home.packages = [ pkgs.nix-output-monitor ];

  programs.bash.initExtra = lib.mkIf config.programs.bash.enable posixWrappers;

  programs.fish.interactiveShellInit = lib.mkIf config.programs.fish.enable (
    lib.concatMapStrings (cli: ''
      if command -q ${cli}
        function ${cli}
          command ${cli} --log-format internal-json -v $argv |& nom --json
        end
      end
    '') clis
  );

  programs.zsh.initContent = lib.mkIf config.programs.zsh.enable posixWrappers;
}
