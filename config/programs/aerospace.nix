{ pkgs, ... }:

{
  programs.aerospace = {
    enable = true;
    package = pkgs.unstable.aerospace;
    launchd = {
      enable = true;
    };

    userSettings = {
      gaps.inner = {
        horizontal = 8;
        vertical = 8;
      };
      gaps.outer = {
        left = 8;
        right = 8;
        top = 8;
        bottom = 8;
      };
      mode = {
        main.binding = {
          cmd-ctrl-slash = "layout tiles horizontal vertical";
          cmd-ctrl-comma = "layout accordion horizontal vertical";
          cmd-ctrl-h = "focus left";
          cmd-ctrl-j = "focus down";
          cmd-ctrl-k = "focus up";
          cmd-ctrl-l = "focus right";
          cmd-ctrl-shift-h = "move left";
          cmd-ctrl-shift-j = "move down";
          cmd-ctrl-shift-k = "move up";
          cmd-ctrl-shift-l = "move right";
          cmd-ctrl-minus = "resize smart -50";
          cmd-ctrl-equal = "resize smart +50";
          cmd-ctrl-1 = "workspace 1";
          cmd-ctrl-2 = "workspace 2";
          cmd-ctrl-3 = "workspace 3";
          cmd-ctrl-4 = "workspace 4";
          cmd-ctrl-5 = "workspace 5";
          cmd-ctrl-6 = "workspace 6";
          cmd-ctrl-7 = "workspace 7";
          cmd-ctrl-8 = "workspace 8";
          cmd-ctrl-9 = "workspace 9";
          cmd-ctrl-shift-1 = "move-node-to-workspace 1";
          cmd-ctrl-shift-2 = "move-node-to-workspace 2";
          cmd-ctrl-shift-3 = "move-node-to-workspace 3";
          cmd-ctrl-shift-4 = "move-node-to-workspace 4";
          cmd-ctrl-shift-5 = "move-node-to-workspace 5";
          cmd-ctrl-shift-6 = "move-node-to-workspace 6";
          cmd-ctrl-shift-7 = "move-node-to-workspace 7";
          cmd-ctrl-shift-8 = "move-node-to-workspace 8";
          cmd-ctrl-shift-9 = "move-node-to-workspace 9";
          cmd-ctrl-tab = "workspace-back-and-forth";
          cmd-ctrl-shift-tab = "move-workspace-to-monitor --wrap-around next";
          cmd-ctrl-shift-semicolon = "mode service";
        };
        service.binding = {
          esc = [ "reload-config" "mode main" ];
          r = [ "flatten-workspace-tree" "mode main" ];
          f = [ "layout floating tiling" "mode main" ];
          backspace = [ "close-all-windows-but-current" "mode main" ];
          cmd-ctrl-shift-h = [ "join-with left" "mode main" ];
          cmd-ctrl-shift-j = [ "join-with down" "mode main" ];
          cmd-ctrl-shift-k = [ "join-with up" "mode main" ];
          cmd-ctrl-shift-l = [ "join-with right" "mode main" ];
        };
      };
    };
  };
}
