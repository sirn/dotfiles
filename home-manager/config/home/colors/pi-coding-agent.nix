{
  themes,
  themeName,
  schemes,
  familyName,
  config,
  lib,
  ...
}:

let
  # Build a Pi theme JSON from a color scheme.
  # Vars hold reusable hex colors; colors references them by name.
  piTheme =
    name: scheme:
    let
      c = scheme.base16Colors;
      s = scheme.semantic;
    in
    {
      "$schema" =
        "https://raw.githubusercontent.com/earendil-works/pi/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json";
      inherit name;
      vars = {
        bg = c.background;
        fg = c.foreground;
        selection = c.selection;
        black = c.normal.black;
        red = c.normal.red;
        green = c.normal.green;
        yellow = c.normal.yellow;
        blue = c.normal.blue;
        magenta = c.normal.magenta;
        cyan = c.normal.cyan;
        white = c.normal.white;
        brightBlack = c.bright.black;
        brightWhite = c.bright.white;
        accent = s.accent.bg;
        secondary = s.secondary.bg;
        tertiary = s.tertiary.bg;
        outline = s.outline;
        muted = s.muted;
        dim = s.dim;
        # Surface backgrounds: lifted neutrals and subtle tints, per scheme.
        surface = s.surface.bg;
        surfaceDim = s.recess.bg;
        successBg = s.success.bg;
        errorBg = s.error.bg;
        customBg = s.surface.bg; # neutral; label carries the color signal
        purple = s.label;
      };
      colors = {
        # Core UI
        accent = "accent";
        border = "blue";
        borderAccent = "cyan";
        borderMuted = "muted";
        success = "green";
        error = "red";
        warning = "yellow";
        muted = "muted";
        dim = "dim";
        text = "fg";
        thinkingText = "muted";

        # Backgrounds & content
        selectedBg = "surface";
        userMessageBg = "surface";
        userMessageText = "fg";
        customMessageBg = "customBg";
        customMessageText = "fg";
        customMessageLabel = "purple";
        toolPendingBg = "surfaceDim";
        toolSuccessBg = "successBg";
        toolErrorBg = "errorBg";
        toolTitle = "fg";
        toolOutput = "muted";

        # Markdown
        mdHeading = "accent";
        mdLink = "blue";
        mdLinkUrl = "muted";
        mdCode = "accent";
        mdCodeBlock = "fg";
        mdCodeBlockBorder = "muted";
        mdQuote = "muted";
        mdQuoteBorder = "outline";
        mdHr = "muted";
        mdListBullet = "accent";

        # Tool diffs
        toolDiffAdded = "green";
        toolDiffRemoved = "red";
        toolDiffContext = "muted";

        # Syntax highlighting
        syntaxComment = "muted";
        syntaxKeyword = "magenta";
        syntaxFunction = "yellow";
        syntaxVariable = "cyan";
        syntaxString = "green";
        syntaxNumber = "red";
        syntaxType = "blue";
        syntaxOperator = "fg";
        syntaxPunctuation = "muted";

        # Thinking level borders (subtle -> prominent)
        thinkingOff = "dim";
        thinkingMinimal = "muted";
        thinkingLow = "blue";
        thinkingMedium = "cyan";
        thinkingHigh = "magenta";
        thinkingXhigh = "red";

        # Bash mode
        bashMode = "yellow";
      };
      export = {
        pageBg = c.background;
        cardBg = s.surface.bg;
        infoBg = s.surface.bg;
      };
    };
in
lib.mkIf config.programs.pi-coding-agent.enable {
  # Pi auto-switches between light and dark using its "light/dark" setting
  # syntax when the terminal appearance changes (detected via COLORFGBG).
  programs.pi-coding-agent.settings.theme =
    if config.home.colors.variants.terminal == "auto" then
      "${schemes.${familyName}.light.name}/${schemes.${familyName}.dark.name}"
    else
      themeName;

  home.file = lib.mapAttrs' (
    name: scheme:
    lib.nameValuePair ".pi/agent/themes/${name}.json" { text = builtins.toJSON (piTheme name scheme); }
  ) themes;
}
