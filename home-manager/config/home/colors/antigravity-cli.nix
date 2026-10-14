{ config, lib, ... }:

lib.mkIf config.programs.antigravity-cli.enable {
  programs.antigravity-cli.settingsOverride.colorScheme =
    if config.home.colors.variants.terminal == "auto" then
      "terminal"
    else
      config.home.colors.variants.terminal;
}
