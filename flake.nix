{
  description = "Home Manager configuration of Sirn";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      overlays = [
        (final: prev: { local = import ./nixpkgs/pkgs { pkgs = prev; }; })
        (final: prev: { unstable = nixpkgs-unstable.legacyPackages.${prev.system}; })
      ];

      defaultConfig = {
        nixpkgs.overlays = overlays;
        nixpkgs.config.allowUnfree = true;
        programs.home-manager.enable = true;
      };
    in
    {
      homeConfigurations = {
        sirn = home-manager.lib.homeManagerConfiguration {
          inherit system pkgs;

          username = "sirn";
          homeDirectory = "/home/sirn";
          stateVersion = "22.05";

          configuration = {
            imports = [
              defaultConfig
              ./nixpkgs/modules/home/email.nix
              ./nixpkgs/modules/home/fonts.nix
              ./nixpkgs/modules/home/links.nix
              ./nixpkgs/modules/home/packages.nix
              ./nixpkgs/modules/programs/aria2.nix
              ./nixpkgs/modules/programs/emacs.nix
              ./nixpkgs/modules/programs/fzf.nix
              ./nixpkgs/modules/programs/git.nix
              ./nixpkgs/modules/programs/gpg.nix
              ./nixpkgs/modules/programs/mbsync.nix
              ./nixpkgs/modules/programs/msmtp.nix
              ./nixpkgs/modules/programs/notmuch.nix
              ./nixpkgs/modules/programs/pandoc.nix
              ./nixpkgs/modules/programs/password-store.nix
              ./nixpkgs/modules/programs/tmux.nix
              ./nixpkgs/modules/programs/vim.nix
              ./nixpkgs/modules/runit-user/duplicity.nix
              ./nixpkgs/modules/runit-user/emacs.nix
              ./nixpkgs/modules/runit-user/gpg-agent.nix
              ./nixpkgs/modules/runit-user/notmuch.nix
              ./nixpkgs/modules/runit-user/xlocate.nix
              ./nixpkgs/modules/dev/ansible.nix
              ./nixpkgs/modules/dev/cloudtools.nix
              ./nixpkgs/modules/dev/containers.nix
              ./nixpkgs/modules/dev/erlang.nix
              ./nixpkgs/modules/dev/go.nix
              ./nixpkgs/modules/dev/kubernetes.nix
              ./nixpkgs/modules/dev/nim.nix
              ./nixpkgs/modules/dev/nix.nix
              ./nixpkgs/modules/dev/nodejs.nix
              ./nixpkgs/modules/dev/nomad.nix
              ./nixpkgs/modules/dev/python.nix
              ./nixpkgs/modules/dev/ruby.nix
              ./nixpkgs/modules/dev/rust.nix
              ./nixpkgs/modules/dev/shell.nix
              ./nixpkgs/modules/dev/sops.nix
              ./nixpkgs/modules/dev/tcl.nix
              ./nixpkgs/modules/dev/terraform.nix
              ./nixpkgs/modules/dev/tools.nix
              ./nixpkgs/modules/dev/vault.nix
              ./nixpkgs/modules/dev/virtualization.nix
            ];
          };
        };
      };
    };
}
