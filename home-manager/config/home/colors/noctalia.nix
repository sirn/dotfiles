{
  theme,
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
      colorSchemes = {
        darkMode = config.home.colors.variant == "dark";
      };
    };
    customPalettes = {
      custom = {
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
      };
    };
  };
}
