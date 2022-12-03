{
  imports = [
    ../profiles/common.nix
    ../profiles/devops.nix
    ../profiles/mail.nix
    ../profiles/dev.nix

    # machine-specific
    ../home/runit.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/notmuch.nix
    ../runit/xlocate.nix
  ];
}
