# Tmux Keybindings

This document summarizes the custom keybindings configured for tmux.

The prefix key is `C-b` (Control+b), referred to as `Prefix` in this document.

## Prefix Keybindings

These keybindings are available after pressing `Prefix`.

| Key | Action                                  |
|:----|:----------------------------------------|
| `r` | Reload the tmux configuration file.     |
| `X` | Resize pane to 85% of the window width. |
| `Y` | Resize pane to 85% of the window height.|

## Copy Mode Keybindings

These bindings are active in copy mode (e.g., `Prefix` `[`).

### vi Mode

| Key | Action                                                    |
|:----|:----------------------------------------------------------|
| `v` | Begin selection.                                          |
| `y` | Copy selection to system clipboard and exit (macOS only). |

### Emacs Mode

| Key   | Action                                                    |
|:------|:----------------------------------------------------------|
| `M-w` | Copy selection to system clipboard and exit (macOS only). |
