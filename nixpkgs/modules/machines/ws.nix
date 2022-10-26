{
  imports = [
    ../profiles/common.nix
    ../profiles/devops.nix
    ../profiles/mail.nix
    ../profiles/dev.nix

    # services
    ../runit/duplicity.nix
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/notmuch.nix
    ../runit/xlocate.nix
  ];
}
