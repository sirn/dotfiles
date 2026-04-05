{
  programs.mosh = {
    enable = true;
    withUtempter = true;
  };

  networking.firewall = {
    allowedUDPPortRanges = [
      {
        from = 60000;
        to = 61000;
      }
    ];
  };
}
