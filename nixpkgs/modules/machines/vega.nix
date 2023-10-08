{
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/runit.nix
    ../home/sway.nix

    # programs
    ../programs/mpv.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/syncthing.nix
    ../runit/xlocate.nix
  ];

  wayland.windowManager.sway = {
    config = {
      output = {
        "*" = {
          scale = "1.5";
        };
      };
    };
  };
}
