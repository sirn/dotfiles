{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (lib) mkIf;
in
{
  programs.jujutsu = {
    enable = true;

    package = pkgs.unstable.jujutsu;

    settings = {
      user = {
        email = config.programs.git.userEmail;
        name = config.programs.git.userName;
      };

      ui = {
        default-command = "log";
        diff-editor = ":builtin";
        pager = ":builtin";

        # Hack! With, Jujutsu $VISUAL has higher priority
        # than $EDITOR, but we cannot hard-code $EDITOR
        # here due to we need with-editor to override $EDITOR
        editor = "${pkgs.writeScriptBin "jjedit" ''
          #!${pkgs.bash}/bin/bash
          # Forces jujutsu to use $EDITOR
          if [ -n "$EDITOR" ]; then
            exec $EDITOR "$@"
          fi

          exec vim "$@"
        ''}/bin/jjedit";
      };
    };
  };

  # Shell completion based on
  # https://github.com/martinvonz/jj/blob/0690922ca15ced55e417edab806c982b0cc42b84/docs/install-and-setup.md
  programs.bash.initExtra = ''
    source <(${config.programs.jujutsu.package}/bin/jj util completion bash)
  '';

  programs.zsh.initExtra = ''
    autoload -U compinit
    compinit
    source <(${config.programs.jujutsu.package}/bin/jj util completion zsh)
  '';

  programs.fish.interactiveShellInit = ''
    ${config.programs.jujutsu.package}/bin/jj util completion fish | source
  '';
}
