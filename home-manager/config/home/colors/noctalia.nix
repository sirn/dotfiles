{
  schemes,
  familyName,
  config,
  lib,
  ...
}:

let
  # Map a variant's semantic colors onto noctalia's Material tokens.
  materialPalette =
    variant:
    let
      s = variant.semantic;
    in
    {
      mError = s.urgent.bg;
      mOnError = s.urgent.text;
      mOnPrimary = s.focus.text;
      mOnSecondary = s.secondary.text;
      mOnSurface = s.primary.text;
      mOnSurfaceVariant = s.inactive.text;
      mOnTertiary = s.tertiary.text;
      mOnHover = s.hover.text;
      mOutline = s.outline;
      mPrimary = s.focus.bg;
      mSecondary = s.secondary.bg;
      mShadow = s.shadow;
      mSurface = s.primary.bg;
      mHover = s.hover.bg;
      mSurfaceVariant = s.inactive.bg;
      mTertiary = s.tertiary.bg;
      terminal = {
        foreground = variant.base16Colors.foreground;
        background = variant.base16Colors.background;
        cursor = variant.base16Colors.foreground;
        cursorText = variant.base16Colors.background;
        selectionFg = variant.base16Colors.foreground;
        selectionBg = variant.base16Colors.selection;
        normal = variant.base16Colors.normal;
        bright = variant.base16Colors.bright;
      };
    };
in
lib.mkIf config.programs.noctalia.enable {
  programs.noctalia = {
    settings = {
      theme = {
        mode = config.home.colors.variants.desktopFallback;
        source = "custom";
        custom_palette = familyName;
      };
    };
    # One palette per family (light + dark), so switching stays within a family
    # and any defined theme is available as a custom palette.
    customPalettes = lib.mapAttrs (_family: variants: {
      light = materialPalette variants.light;
      dark = materialPalette variants.dark;
    }) schemes;
  };
}
