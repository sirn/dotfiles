{
  imports = [
    ../profiles/common.nix
    ../profiles/devops.nix
    ../profiles/kubeops.nix
    ../profiles/dev.nix

    # email
    ../programs/mbsync.nix
    ../programs/msmtp.nix
    ../programs/notmuch.nix

    # services
    ../runit-user/duplicity.nix
    ../runit-user/emacs.nix
    ../runit-user/gpg-agent.nix
    ../runit-user/notmuch.nix
    ../runit-user/xlocate.nix
  ];
}
