{
  boot.plymouth = {
    enable = true;
  };

  boot.initrd.systemd.enable = true;

  boot.consoleLogLevel = 3;
  boot.initrd.verbose = false;

  boot.kernelParams = [
    "quiet"
    "udev.log_level=3"
    "systemd.show_status=auto"
  ];
}
