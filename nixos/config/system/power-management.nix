{
  # Audio Power Saving
  boot.extraModprobeConfig = ''
    options snd_hda_intel power_save=1
  '';

  # USB Autosuspend and SATA link power management
  boot.kernelParams = [
    "usbcore.autosuspend=120"
    "scsi_mod.use_blk_mq=1"
    "nmi_watchdog=0"
    "nvme_core.default_ps_max_latency_us=6000"
  ];
}
