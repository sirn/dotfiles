{ config, ... }:

{
  sops.age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";

  agents.sandbox.envFiles = [
    "${config.xdg.configHome}/sops-nix/secrets/agents/env"
    "${config.xdg.configHome}/sops-nix/secrets/agents/env.local"
  ];
}
