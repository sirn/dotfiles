{
  services.openssh = {
    enable = true;
    ports = [ 4022 ];
  };

  networking.firewall = {
    allowedTCPPorts = [ 4022 ];
  };
}
