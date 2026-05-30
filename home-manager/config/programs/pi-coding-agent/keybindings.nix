{ ... }:

let
  keybindingMap = {
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
  programs.pi-coding-agent = {
    keybindings = keybindingMap;
  };
}
