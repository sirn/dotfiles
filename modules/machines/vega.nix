{
  machine.runit.enable = true;
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/runit.nix
    ../home/sway.nix

    # programs
    ../programs/emacsc.nix
    ../programs/firefox.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix
    ../programs/s-tui.nix
    ../programs/thunderbird.nix

    # services
    ../services/emacs.nix
    ../services/gpg-agent.nix
    ../services/syncthing.nix
    ../services/xlocate.nix
  ];
}
