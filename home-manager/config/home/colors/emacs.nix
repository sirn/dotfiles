{
  schemes,
  familyName,
  config,
  lib,
  ...
}:

let
  family = schemes.${familyName};

  hasEmacsTheme = family.dark ? emacsTheme && family.light ? emacsTheme;

  modeVars = {
    "desktop-dark" = family.dark.emacsTheme.customElisp;
    "desktop-light" = family.light.emacsTheme.customElisp;
    "terminal-dark" = family.dark.emacsTheme.customElisp;
    "terminal-light" = family.light.emacsTheme.customElisp;
  };

  loaderElisp = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (name: loader: "(setq gemacs-theme-${name} (lambda () ${loader}))") modeVars
  );
in
lib.mkIf (config.programs.emacs.enable && hasEmacsTheme) {
  programs.emacs = {
    afterInitExtra = ''
      ${loaderElisp}
      (setq gemacs-theme-desktop-mode "${config.home.colors.variants.desktop}")
      (setq gemacs-theme-terminal-mode "${config.home.colors.variants.terminal}")
    '';

    themePackages =
      epkgs:
      lib.lists.unique (
        (family.dark.emacsTheme.packages epkgs) ++ (family.light.emacsTheme.packages epkgs)
      );
  };
}
