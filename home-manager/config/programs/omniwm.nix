{ pkgs, ... }:

{
  programs.omniwm = {
    enable = true;
    package = pkgs.local.omniwm;
    launchd.enable = true;

    appearance.mode = "dark";
    borders = {
      color = "#2fafff99";
      width = 4;
    };

    general = {
      defaultLayoutType = "niri";
      ipcEnabled = true;
      updateChecksEnabled = false;
    };

    focus = {
      followsMouse = true;
      followsWindowToMonitor = false;
      moveMouseToFocusedWindow = false;
    };

    niri = {
      centerFocusedColumn = "never";
      alwaysCenterSingleColumn = true;
      maxVisibleColumns = 2;
      maxWindowsPerColumn = 3;
    };

    quakeTerminal.enabled = false;

    workspaceBar = {
      hideEmptyWorkspaces = true;
      position = "overlappingMenuBar";
      notchAware = true;
      backgroundOpacity = 0.1;
    };

    keybindings = {
      # Workspace switching
      "switchWorkspace.0" = "Command+Control+1";
      "switchWorkspace.1" = "Command+Control+2";
      "switchWorkspace.2" = "Command+Control+3";
      "switchWorkspace.3" = "Command+Control+4";
      "switchWorkspace.4" = "Command+Control+5";
      "switchWorkspace.5" = "Command+Control+6";
      "switchWorkspace.6" = "Command+Control+7";
      "switchWorkspace.7" = "Command+Control+8";
      "switchWorkspace.8" = "Command+Control+9";
      "workspaceBackAndForth" = "Command+Control+Tab";

      # Move window to workspace
      "moveToWorkspace.0" = "Command+Control+Shift+1";
      "moveToWorkspace.1" = "Command+Control+Shift+2";
      "moveToWorkspace.2" = "Command+Control+Shift+3";
      "moveToWorkspace.3" = "Command+Control+Shift+4";
      "moveToWorkspace.4" = "Command+Control+Shift+5";
      "moveToWorkspace.5" = "Command+Control+Shift+6";
      "moveToWorkspace.6" = "Command+Control+Shift+7";
      "moveToWorkspace.7" = "Command+Control+Shift+8";
      "moveToWorkspace.8" = "Command+Control+Shift+9";

      # Focus (vim-style)
      "focus.left" = "Command+Control+H";
      "focus.down" = "Command+Control+J";
      "focus.up" = "Command+Control+K";
      "focus.right" = "Command+Control+L";
      "focusPrevious" = "Command+Control+Option+Tab";

      # Move window (vim-style)
      "move.left" = "Command+Control+[";
      "move.down" = "Command+Control+Shift+J";
      "move.up" = "Command+Control+Shift+K";
      "move.right" = "Command+Control+]";

      # Move column (vim-style)
      "moveColumn.left" = "Command+Control+Shift+H";
      "moveColumn.right" = "Command+Control+Shift+L";

      # Move window across workspaces
      "moveWindowToWorkspaceUp" = "Command+Control+Shift+Up Arrow";
      "moveWindowToWorkspaceDown" = "Command+Control+Shift+Down Arrow";
      "moveColumnToWorkspaceUp" = "Command+Control+Shift+Page Up";
      "moveColumnToWorkspaceDown" = "Command+Control+Shift+Page Down";

      # Monitor focus
      "focusMonitorNext" = "Command+Control+Shift+Tab";
      "focusMonitorLast" = "Command+Control+Option+\`";

      # Layout
      "toggleFullscreen" = "Command+Control+Return";
      "toggleColumnTabbed" = "Command+Control+T";
      "toggleColumnFullWidth" = "Command+Control+Shift+F";
      "toggleWorkspaceLayout" = "Command+Control+Shift+W";
      "toggleOverview" = "Command+Control+Shift+O";
      "balanceSizes" = "Command+Control+Shift+B";

      # Column navigation
      "focusColumnFirst" = "Command+Control+Option+Home";
      "focusColumnLast" = "Command+Control+Option+End";
      "focusColumn.0" = "Command+Control+Option+1";
      "focusColumn.1" = "Command+Control+Option+2";
      "focusColumn.2" = "Command+Control+Option+3";
      "focusColumn.3" = "Command+Control+Option+4";
      "focusColumn.4" = "Command+Control+Option+5";
      "focusColumn.5" = "Command+Control+Option+6";
      "focusColumn.6" = "Command+Control+Option+7";
      "focusColumn.7" = "Command+Control+Option+8";
      "focusColumn.8" = "Command+Control+Option+9";
      "cycleColumnWidthForward" = "Command+Control+.";
      "cycleColumnWidthBackward" = "Command+Control+,";


      # UI
      "openCommandPalette" = "Command+Control+Space";
      "openMenuAnywhere" = "Command+Control+M";
      "raiseAllFloatingWindows" = "Command+Control+Shift+R";
    };

    workspaces = [
      { name = "1"; }
      { name = "2"; }
      { name = "3"; }
      { name = "4"; }
    ];
  };
}
