{
  services.sanoid = {
    enable = true;

    templates = {
      hourly = {
        frequently = 0;
        hourly = 24;
        daily = 7;
        weekly = 4;
        monthly = 1;
        yearly = 0;
        autosnap = true;
        autoprune = true;
      };

      daily = {
        frequently = 0;
        hourly = 0;
        daily = 7;
        weekly = 4;
        monthly = 1;
        yearly = 0;
        autosnap = true;
        autoprune = true;
      };

      ignore = {
        autoprune = false;
        autosnap = false;
        monitor = false;
      };
    };
  };
}
