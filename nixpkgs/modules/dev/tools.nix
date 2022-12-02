{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  home.packages = with pkgs; [
    # dev
    bmake
    entr
    jq
    mercurial
    yamllint
    yq-go

    # net
    curlFull
    ipcalc
    mosh
    openssh
    rsync
    socat

    # sys
    bzip2
    tree
    unzip
    xz
    zip

    # textproc
    aspell
    proselint
    silver-searcher
  ] ++ (if isDarwin then [ ] else [
    # Note: duplicity has an actual dependency on pyOpenSSL which is
    # broken on aarch64-darwin. Broken until the issue is resolved
    # upstream.
    #
    # Last check: nixpkgs-unstable 20221027
    #
    # See also: https://github.com/pyca/pyopenssl/issues/873
    duplicity
  ]);

  home.file = {
    ".proselintrc" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/proselint/proselintrc";
    };

    ".mosh_init" = {
      executable = true;
      text = ''
        #!/bin/sh -l
        exec ${pkgs.tmux}/bin/tmux new-session -A -s main
      '';
    };
  };
}
