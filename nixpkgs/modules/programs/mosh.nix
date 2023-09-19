{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    mosh
  ];

  home.file = {
    ".mosh_init" = {
      executable = true;
      text = ''
        #!/bin/sh -l
        exec ${pkgs.tmux}/bin/tmux new-session -A -s main
      '';
    };
  };
}
