{
  schemes,
  familyName,
  config,
  lib,
  ...
}:

let
  # Render a jjui style inline table, e.g. { fg = "#fff", bg = "#000", bold = true }
  fmtVal =
    v:
    if builtins.isBool v then
      lib.boolToString v
    else if builtins.isString v then
      "\"${v}\""
    else
      toString v;

  styleLine =
    selector: style:
    "\"${selector}\" = { "
    + lib.concatStringsSep ", " (lib.mapAttrsToList (k: v: "${k} = ${fmtVal v}") style)
    + " }";

  # Map a variant's semantic colors onto jjui's theme selectors.
  jjuiTheme =
    variant:
    let
      s = variant.semantic;
    in
    {
      # Global
      "text" = {
        fg = s.primary.text;
        bg = s.primary.bg;
      };
      "dimmed" = {
        fg = s.dim;
      };
      "selected" = {
        bg = s.inactive.bg;
        fg = s.primary.text;
      };
      "border" = {
        fg = s.outline;
      };
      "title" = {
        fg = s.label;
        bold = true;
      };
      "shortcut" = {
        fg = s.tertiary.bg;
      };
      "matched" = {
        fg = s.accent.bg;
        underline = true;
      };
      "source_marker" = {
        bg = s.warning.bg;
        fg = s.warning.text;
      };
      "target_marker" = {
        bg = s.success.bg;
        fg = s.success.text;
      };

      # Revset (top bar)
      "revset title" = {
        fg = s.muted;
      };
      "revset text" = {
        bold = true;
      };
      "revset completion selected" = {
        bg = s.inactive.bg;
        fg = s.primary.text;
      };
      "revset completion matched" = {
        fg = s.accent.bg;
        bold = true;
      };
      "revset completion dimmed" = {
        fg = s.dim;
      };

      # Revisions / oplog
      "revisions selected" = {
        bold = true;
      };
      "revisions dimmed" = {
        fg = s.dim;
      };

      # Status (bottom bar)
      "status" = {
        bg = s.surface.bg;
        fg = s.surface.text;
      };
      "status title" = {
        bg = s.inactive.bg;
        fg = s.primary.text;
        bold = true;
      };
      "status shortcut" = {
        fg = s.tertiary.bg;
      };
      "status dimmed" = {
        fg = s.dim;
      };

      # Evolog
      "evolog selected" = {
        bold = true;
      };

      # Menus
      "menu" = {
        bg = s.surface.bg;
        fg = s.surface.text;
      };
      "menu title" = {
        fg = s.label;
        bold = true;
      };
      "menu shortcut" = {
        fg = s.tertiary.bg;
      };
      "menu dimmed" = {
        fg = s.dim;
      };
      "menu border" = {
        fg = s.outline;
      };
      "menu selected" = {
        bg = s.inactive.bg;
        fg = s.primary.text;
      };

      # Help
      "help" = {
        bg = s.surface.bg;
      };
      "help title" = {
        fg = s.label;
        bold = true;
        underline = true;
      };
      "help border" = {
        fg = s.outline;
      };

      # Confirmation
      "confirmation" = {
        bg = s.surface.bg;
      };
      "confirmation text" = {
        fg = s.primary.text;
      };
      "confirmation selected" = {
        bg = s.inactive.bg;
        fg = s.primary.text;
      };
      "confirmation dimmed" = {
        fg = s.dim;
      };
      "confirmation border" = {
        fg = s.outline;
      };

      # Undo
      "undo" = {
        bg = s.surface.bg;
      };
      "undo confirmation dimmed" = {
        fg = s.dim;
      };
      "undo confirmation selected" = {
        bg = s.inactive.bg;
        fg = s.primary.text;
      };

      # Preview
      "preview" = {
        fg = s.primary.text;
      };
      "preview border" = {
        fg = s.outline;
      };
    };

  themeText =
    variant:
    let
      theme = jjuiTheme variant;
    in
    lib.concatStringsSep "\n" (lib.mapAttrsToList styleLine theme) + "\n";
in
lib.mkIf config.programs.jjui.enable {
  programs.jjui.settings.ui.theme = {
    light = schemes.${familyName}.light.name;
    dark = schemes.${familyName}.dark.name;
  };

  # One theme file per variant (keyed by the variant's own theme name),
  # so any defined scheme is available and jjui can switch between
  # light/dark at runtime.
  xdg.configFile = lib.concatMapAttrs (
    family: variants:
    lib.mapAttrs' (
      variant: theme: lib.nameValuePair "jjui/themes/${theme.name}.toml" { text = themeText theme; }
    ) variants
  ) schemes;
}
