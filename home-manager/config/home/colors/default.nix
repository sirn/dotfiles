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

  # Auto-discover theme files from schemes directory
  themeFiles = builtins.readDir ./schemes;
  themeNames = builtins.map (lib.removeSuffix ".nix") (
    builtins.attrNames (
      lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".nix" name) themeFiles
    )
  );

  # Import all discovered themes
  themes = lib.genAttrs themeNames (
    name: import (./schemes + "/${name}.nix") { inherit lib generatePalette; }
  );

  # Selected theme for single-active consumers
  themeName = config.home.colors.themeName;
  theme = themes.${themeName};
  variant = theme.variant;

  stripHash = color: builtins.substring 1 6 color;

  # Shared args for per-program color configs
  args = {
    inherit
      config
      lib
      theme
      themes
      themeName
      stripHash
      ;
  };

in
lib.mkMerge [
  { home.colors.variant = variant; }
  (import ./alacritty.nix args)
  (import ./emacs.nix args)
  (import ./foot.nix args)
  (import ./fuzzel.nix args)
  (import ./ghostty.nix args)
  (import ./mako.nix args)
  (import ./niri.nix args)
  (import ./noctalia.nix args)
  (import ./sway.nix args)
  (import ./swaylock.nix args)
  (import ./waybar.nix args)
  (import ./wezterm.nix args)
]
