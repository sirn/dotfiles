{
  services.syncoid = {
    enable = true;
    interval = "*-*-* 2:00:00";

    commonArgs = [
      "--no-sync-snap"
      "--compress=zstd-slow"
      "--no-privilege-elevation"
      "--delete-target-snapshots"
    ];
  };
}
