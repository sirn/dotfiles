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
    ../programs/emacsc.nix
    ../programs/firefox.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix
    ../programs/obsidian.nix
    ../programs/thunderbird.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/syncthing.nix
    ../runit/xlocate.nix
  ];
}
