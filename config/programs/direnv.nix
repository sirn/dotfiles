{ config, pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    stdlib = ''
      direnv_layout_dir() {
        local cache_dir="''${XDG_CACHE_HOME:-$HOME/.cache}/direnv/layouts"
        local hash; hash="$(echo -n "$PWD" | sha1sum | head -c 40)"
        echo "''${cache_dir}/''${hash}"
      }
    '';
  };
}
