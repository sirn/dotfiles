{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  home.packages = with pkgs; [
    # dev
    autoconf
    automake
    bmake
    buf
    entr
    execline
    jq
    jsonnet
    mercurial
    yamllint
    yq-go

    # db
    postgresql_14
    redis
    sqlite

    # net
    amfora
    autossh
    curlFull
    ipcalc
    mosh
    openssh
    rsync
    socat
    unison
    w3m-nox

    # sys
    bzip2
    duplicity
    execline
    honcho
    s6
    tree
    unzip
    xz
    zip

    # textproc
    aspell
    mdbook
    proselint
    silver-searcher
  ];

  home.file = {
    ".proselintrc" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/proselint/proselintrc";
    };
  };
}
