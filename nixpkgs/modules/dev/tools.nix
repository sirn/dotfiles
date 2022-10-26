{ config, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isDarwin;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  home.packages = with pkgs; [
    # dev
    entr
    jq
    mercurial
    yamllint
    yq-go
    unstable.bmake

    # net
    curlFull
    ipcalc
    mosh
    openssh
    rsync
    socat
    unison

    # sys
    bzip2
    execline
    honcho
    s6
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
  };
}
