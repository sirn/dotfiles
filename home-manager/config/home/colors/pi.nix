{
  themes,
  themeName,
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
      tui = s.tui;
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
        accent = s.focus.bg;
        secondary = s.secondary.bg;
        tertiary = s.tertiary.bg;
        outline = s.outline;
        muted = tui.muted;
        dim = tui.dim;
        # Surface backgrounds: lifted neutrals and subtle tints, per scheme.
        surface = tui.surface;
        surfaceDim = tui.recess;
        successBg = tui.success;
        errorBg = tui.error;
        customBg = tui.surface; # neutral; label carries the color signal
        purple = tui.label;
      };
      colors = {
        # Core UI
        accent = "accent";
        border = "blue";
        borderAccent = "cyan";
        borderMuted = "brightBlack";
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
        mdLinkUrl = "dim";
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
        syntaxOperator = "brightWhite";
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
        cardBg = tui.surface;
        infoBg = tui.surface;
      };
    };
in
lib.mkIf config.programs.pi-coding-agent.enable {
  programs.pi-coding-agent.settings.theme = themeName;

  home.file = lib.mapAttrs' (
    name: scheme:
    lib.nameValuePair ".pi/agent/themes/${name}.json" { text = builtins.toJSON (piTheme name scheme); }
  ) themes;
}
