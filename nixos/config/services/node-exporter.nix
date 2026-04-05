{
  services.prometheus.exporters.node = {
    enable = true;
    user = "root"; # for rapl, etc.
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = "node";
      static_configs = [ { targets = [ "localhost:9100" ]; } ];
    }
  ];
}
