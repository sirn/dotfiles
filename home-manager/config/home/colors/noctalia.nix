{
  theme,
  themeName,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.programs.noctalia.enable {
  programs.noctalia = {
    settings = {
      theme = {
        mode = config.home.colors.variant;
        source = "custom";
        custom_palette = themeName;
      };
    };
    customPalettes = {
      "${themeName}" = {
        dark = {
          mError = semantic.urgent.bg;
          mOnError = semantic.urgent.text;
          mOnPrimary = semantic.focus.text;
          mOnSecondary = semantic.secondary.text;
          mOnSurface = semantic.primary.text;
          mOnSurfaceVariant = semantic.inactive.text;
          mOnTertiary = semantic.tertiary.text;
          mOnHover = semantic.hover.text;
          mOutline = semantic.outline;
          mPrimary = semantic.focus.bg;
          mSecondary = semantic.secondary.bg;
          mShadow = semantic.shadow;
          mSurface = semantic.primary.bg;
          mHover = semantic.hover.bg;
          mSurfaceVariant = semantic.inactive.bg;
          mTertiary = semantic.tertiary.bg;
          terminal = {
            foreground = theme.base16Colors.foreground;
            background = theme.base16Colors.background;
            cursor = theme.base16Colors.foreground;
            cursorText = theme.base16Colors.background;
            selectionFg = theme.base16Colors.foreground;
            selectionBg = theme.base16Colors.selection;
            normal = theme.base16Colors.normal;
            bright = theme.base16Colors.bright;
          };
        };
      };
    };
  };
}
