{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Palette generator function passed to themes
  generatePalette =
    name: base16Colors:
    let
      palette240File =
        pkgs.runCommand "generate-palette-256-${name}" { nativeBuildInputs = [ pkgs.python3 ]; }
          ''
            cat > input.json <<'EOF'
            {
              "bg": "${base16Colors.background}",
              "fg": "${base16Colors.foreground}",
              "normal": [
                "${base16Colors.normal.black}",
                "${base16Colors.normal.red}",
                "${base16Colors.normal.green}",
                "${base16Colors.normal.yellow}",
                "${base16Colors.normal.blue}",
                "${base16Colors.normal.magenta}",
                "${base16Colors.normal.cyan}",
                "${base16Colors.normal.white}"
              ]
            }
            EOF
            python3 ${./generate-palette.py} < input.json > $out
          '';

      palette240 = builtins.fromJSON (builtins.readFile palette240File);

      base16List = [
        base16Colors.normal.black
        base16Colors.normal.red
        base16Colors.normal.green
        base16Colors.normal.yellow
        base16Colors.normal.blue
        base16Colors.normal.magenta
        base16Colors.normal.cyan
        base16Colors.normal.white
        base16Colors.bright.black
        base16Colors.bright.red
        base16Colors.bright.green
        base16Colors.bright.yellow
        base16Colors.bright.blue
        base16Colors.bright.magenta
        base16Colors.bright.cyan
        base16Colors.bright.white
      ];
    in
    base16List ++ palette240;

  # Auto-discover scheme files; each defines { light = {...}; dark = {...}; }
  schemeFiles = builtins.readDir ./schemes;
  schemeNames = builtins.map (lib.removeSuffix ".nix") (
    builtins.attrNames (
      lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".nix" name) schemeFiles
    )
  );

  # Nested by family: schemes.<family>.{light,dark} for light/dark-switching tools
  schemes = lib.genAttrs schemeNames (
    name: import (./schemes + "/${name}.nix") { inherit lib generatePalette; }
  );

  # Flattened by each variant's formal name, tagged with variant + family.
  themes = lib.listToAttrs (
    lib.flatten (
      lib.mapAttrsToList (
        family: variants:
        lib.mapAttrsToList (
          variant: scheme: lib.nameValuePair scheme.name (scheme // { inherit variant family; })
        ) variants
      ) schemes
    )
  );

  # Resolve active themes per context
  familyName = config.home.colors.themeName;
  desktopVariant = config.home.colors.variants.desktop;
  terminalVariant = config.home.colors.variants.terminal;

  desktopTheme = schemes.${familyName}.${desktopVariant};
  terminalTheme = schemes.${familyName}.${terminalVariant};
  desktopThemeName = desktopTheme.name;
  terminalThemeName = terminalTheme.name;

  stripHash = color: builtins.substring 1 6 color;

  # Shared args for per-program color configs
  desktopArgs = {
    inherit
      config
      lib
      themes
      schemes
      familyName
      stripHash
      ;
    theme = desktopTheme;
    themeName = desktopThemeName;
  };

  terminalArgs = {
    inherit
      config
      lib
      themes
      schemes
      familyName
      stripHash
      ;
    theme = terminalTheme;
    themeName = terminalThemeName;
  };

in
lib.mkMerge [
  {
    home.colors.desktopThemeName = desktopThemeName;
    home.colors.terminalThemeName = terminalThemeName;
  }
  (import ./claude-code.nix terminalArgs)
  (import ./coord.nix desktopArgs)
  (import ./alacritty.nix terminalArgs)
  (import ./emacs.nix terminalArgs)
  (import ./fizzterm.nix terminalArgs)
  (import ./foot.nix terminalArgs)
  (import ./fuzzel.nix desktopArgs)
  (import ./ghostty.nix terminalArgs)
  (import ./jjui.nix terminalArgs)
  (import ./localterm.nix terminalArgs)
  (import ./mako.nix desktopArgs)
  (import ./niri.nix desktopArgs)
  (import ./noctalia.nix desktopArgs)
  (import ./pi-coding-agent.nix terminalArgs)
  (import ./sway.nix desktopArgs)
  (import ./swaylock.nix desktopArgs)
  (import ./waybar.nix desktopArgs)
  (import ./wezterm.nix terminalArgs)
]
