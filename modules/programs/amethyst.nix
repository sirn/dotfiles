{ lib, ... }:

{
  home.file = {
    ".amethyst.yml" = {
      text = lib.generators.toYAML { } {
        layouts = [ "tall" "wide" "fullscreen" "column" ];
        mouse-follows-focus = false;
        focus-follows-mouse = true;

        floating = [
          "com.adobe.illustrator"
          "com.adobe.InDesign"
          "com.adobe.Photoshop"
          "org.blenderfoundation.blender"
        ];
      };
    };
  };
}
