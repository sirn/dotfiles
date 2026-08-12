{ config, lib, ... }:

lib.mkIf config.programs.claude-code.enable {
  programs.claude-code.settings.theme = config.home.colors.variants.terminalFallback;
}
