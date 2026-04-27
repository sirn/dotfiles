{
  theme,
  config,
  lib,
  ...
}:

lib.mkIf config.programs.emacs.enable {
  programs.emacs = {
    afterInitExtra = theme.emacsTheme.customElisp;
    themePackages = theme.emacsTheme.packages;
  };
}
