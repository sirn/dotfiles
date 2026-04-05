{
  imports = [ ./prometheus.nix ];

  services.prometheus = {
    enableAgentMode = true;
  };
}
