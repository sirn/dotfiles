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

  # ANSI theme: drive the UI from the terminal's own palette so pi follows
  # the terminal when it switches appearance (auto). Empty strings mean
  # terminal default; numeric values are ANSI palette indices.
  ansiTheme = {
    "$schema" =
      "https://raw.githubusercontent.com/earendil-works/pi/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json";
    name = "ansi";
    vars = {
      black = 0;
      red = 1;
      green = 2;
      yellow = 3;
      blue = 4;
      magenta = 5;
      cyan = 6;
      white = 7;
      brightBlack = 8;
      brightRed = 9;
      brightGreen = 10;
      brightYellow = 11;
      brightBlue = 12;
      brightMagenta = 13;
      brightCyan = 14;
      brightWhite = 15;
    };
    colors = {
      accent = "brightBlue";
      border = "brightBlack";
      borderAccent = "brightBlue";
      borderMuted = "black";
      success = "green";
      error = "red";
      warning = "yellow";
      muted = "brightBlack";
      dim = "black";
      text = "";
      thinkingText = "brightBlack";

      # Default terminal background so surfaces follow the terminal.
      selectedBg = "";
      userMessageBg = "";
      userMessageText = "";
      customMessageBg = "";
      customMessageText = "";
      customMessageLabel = "brightMagenta";
      toolPendingBg = "";
      toolSuccessBg = "";
      toolErrorBg = "";
      toolTitle = "white";
      toolOutput = "brightBlack";

      # Markdown
      mdHeading = "brightYellow";
      mdLink = "brightBlue";
      mdLinkUrl = "brightBlack";
      mdCode = "brightCyan";
      mdCodeBlock = "";
      mdCodeBlockBorder = "brightBlack";
      mdQuote = "brightBlack";
      mdQuoteBorder = "brightBlack";
      mdHr = "brightBlack";
      mdListBullet = "brightCyan";

      # Tool diffs
      toolDiffAdded = "green";
      toolDiffRemoved = "red";
      toolDiffContext = "brightBlack";

      # Syntax highlighting
      syntaxComment = "brightBlack";
      syntaxKeyword = "magenta";
      syntaxFunction = "brightYellow";
      syntaxVariable = "brightCyan";
      syntaxString = "green";
      syntaxNumber = "red";
      syntaxType = "brightBlue";
      syntaxOperator = "";
      syntaxPunctuation = "brightBlack";

      # Thinking level borders (subtle -> prominent)
      thinkingOff = "black";
      thinkingMinimal = "brightBlack";
      thinkingLow = "blue";
      thinkingMedium = "cyan";
      thinkingHigh = "magenta";
      thinkingXhigh = "red";

      # Bash mode
      bashMode = "brightYellow";
    };
  };
in
lib.mkIf config.programs.pi-coding-agent.enable {
  programs.pi-coding-agent.settings.theme =
    if config.home.colors.variants.terminal == "auto" then "ansi" else themeName;

  home.file =
    lib.mapAttrs' (
      name: scheme:
      lib.nameValuePair ".pi/agent/themes/${name}.json" { text = builtins.toJSON (piTheme name scheme); }
    ) themes
    // {
      ".pi/agent/themes/ansi.json".text = builtins.toJSON ansiTheme;
    };
}
