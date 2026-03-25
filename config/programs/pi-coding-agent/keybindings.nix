# Keybindings configuration for Pi coding agent
# Format changed in Pi 0.61.0 (2026-03-20):
# - < 0.61.0: simple names like "newLine", "cursorUp"
# - >= 0.61.0: namespaced ids like "tui.input.newLine", "tui.editor.cursorUp"

{
  config,
  lib,
  pkgs,
  isPi061orLater,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;

  # Pre-0.61.0 format (simple names)
  keybindingsV58 = {
    # Cursor Movement (Emacs)
    "cursorUp" = [
      "up"
      "ctrl+p"
    ];
    "cursorDown" = [
      "down"
      "ctrl+n"
    ];
    "cursorLeft" = [
      "left"
      "ctrl+b"
    ];
    "cursorRight" = [
      "right"
      "ctrl+f"
    ];
    "cursorWordLeft" = [
      "alt+left"
      "ctrl+left"
      "alt+b"
    ];
    "cursorWordRight" = [
      "alt+right"
      "ctrl+right"
      "alt+f"
    ];
    "cursorLineStart" = [
      "home"
      "ctrl+a"
    ];
    "cursorLineEnd" = [
      "end"
      "ctrl+e"
    ];

    # Deletion (Emacs)
    "deleteCharBackward" = [
      "backspace"
      "ctrl+h"
    ];
    "deleteCharForward" = [
      "delete"
      "ctrl+d"
    ];
    "deleteWordBackward" = [
      "ctrl+w"
      "alt+backspace"
    ];
    "deleteWordForward" = [
      "alt+d"
      "alt+delete"
    ];
    "deleteToLineStart" = [ "ctrl+u" ];
    "deleteToLineEnd" = [ "ctrl+k" ];

    # Text Input
    "newLine" = [
      "shift+enter"
      "ctrl+j"
    ];
    "submit" = [ "enter" ];
    "tab" = [ "tab" ];

    # Selection (for ctx.ui.select dialogs)
    "selectUp" = [
      "up"
      "ctrl+p"
    ];
    "selectDown" = [
      "down"
      "ctrl+n"
    ];
    "selectConfirm" = [ "enter" ];
    "selectCancel" = [
      "escape"
      "ctrl+c"
    ];

    # Tree Navigation (session tree view)
    "treeFoldOrUp" = [
      "ctrl+left"
      "alt+left"
    ];
    "treeUnfoldOrDown" = [
      "ctrl+right"
      "alt+right"
    ];

    # Kill Ring (Emacs)
    "yank" = [ "ctrl+y" ];
    "yankPop" = [ "alt+y" ];
    "undo" = [
      "ctrl+_"
      "ctrl+/"
    ];

    # Application
    "interrupt" = [ "escape" ];
    "clear" = [ "ctrl+c" ];
    "exit" = [ "ctrl+d" ];
    "externalEditor" = [ "ctrl+g" ];

    # Models and Thinking
    "selectModel" = [ "ctrl+l" ];
    "cycleModelForward" = [ "ctrl+period" ];
    "cycleModelBackward" = [ "ctrl+comma" ];
    "cycleThinkingLevel" = [ "shift+tab" ];

    # Display
    "expandTools" = [ "ctrl+o" ];
    "toggleThinking" = [ "ctrl+t" ];

    # Message Queue
    "followUp" = [ "alt+enter" ];
    "dequeue" = [ "alt+up" ];
  };

  # 0.61.0+ format (namespaced ids)
  keybindingsV061 = {
    # Cursor Movement (Emacs)
    "tui.editor.cursorUp" = [
      "up"
      "ctrl+p"
    ];
    "tui.editor.cursorDown" = [
      "down"
      "ctrl+n"
    ];
    "tui.editor.cursorLeft" = [
      "left"
      "ctrl+b"
    ];
    "tui.editor.cursorRight" = [
      "right"
      "ctrl+f"
    ];
    "tui.editor.cursorWordLeft" = [
      "alt+left"
      "ctrl+left"
      "alt+b"
    ];
    "tui.editor.cursorWordRight" = [
      "alt+right"
      "ctrl+right"
      "alt+f"
    ];
    "tui.editor.cursorLineStart" = [
      "home"
      "ctrl+a"
    ];
    "tui.editor.cursorLineEnd" = [
      "end"
      "ctrl+e"
    ];

    # Deletion (Emacs)
    "tui.editor.deleteCharBackward" = [
      "backspace"
      "ctrl+h"
    ];
    "tui.editor.deleteCharForward" = [
      "delete"
      "ctrl+d"
    ];
    "tui.editor.deleteWordBackward" = [
      "ctrl+w"
      "alt+backspace"
    ];
    "tui.editor.deleteWordForward" = [
      "alt+d"
      "alt+delete"
    ];
    "tui.editor.deleteToLineStart" = [ "ctrl+u" ];
    "tui.editor.deleteToLineEnd" = [ "ctrl+k" ];

    # Text Input
    "tui.input.newLine" = [
      "shift+enter"
      "ctrl+j"
    ];
    "tui.input.submit" = [ "enter" ];
    "tui.input.tab" = [ "tab" ];

    # Selection (for ctx.ui.select dialogs)
    "tui.select.up" = [
      "up"
      "ctrl+p"
    ];
    "tui.select.down" = [
      "down"
      "ctrl+n"
    ];
    "tui.select.confirm" = [ "enter" ];
    "tui.select.cancel" = [
      "escape"
      "ctrl+c"
    ];

    # Tree Navigation (session tree view)
    "app.tree.foldOrUp" = [
      "ctrl+left"
      "alt+left"
    ];
    "app.tree.unfoldOrDown" = [
      "ctrl+right"
      "alt+right"
    ];

    # Kill Ring (Emacs)
    "tui.editor.yank" = [ "ctrl+y" ];
    "tui.editor.yankPop" = [ "alt+y" ];
    "tui.editor.undo" = [
      "ctrl+_"
      "ctrl+/"
    ];

    # Application
    "app.interrupt" = [ "escape" ];
    "app.clear" = [ "ctrl+c" ];
    "app.exit" = [ "ctrl+d" ];
    "app.editor.external" = [ "ctrl+g" ];

    # Models and Thinking
    "app.model.select" = [ "ctrl+l" ];
    "app.model.cycleForward" = [ "ctrl+period" ];
    "app.model.cycleBackward" = [ "ctrl+comma" ];
    "app.thinking.cycle" = [ "shift+tab" ];

    # Display
    "app.tools.expand" = [ "ctrl+o" ];
    "app.thinking.toggle" = [ "ctrl+t" ];

    # Message Queue
    "app.message.followUp" = [ "alt+enter" ];
    "app.message.dequeue" = [ "alt+up" ];
  };
in
{
  programs.pi-coding-agent = lib.mkIf cfg.enable {
    keybindings = if isPi061orLater then keybindingsV061 else keybindingsV58;
  };
}
