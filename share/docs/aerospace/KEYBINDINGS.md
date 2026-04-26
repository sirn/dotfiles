# AeroSpace Keybindings

This document summarizes the keybindings configured for the AeroSpace tiling window manager (macOS).

The modifier key is `Cmd+Ctrl` (Command+Control), referred to as `Mod` in this document.

## Main Mode

These keybindings are available in the default (main) mode.

### Layouts

| Keybinding | Action                                          |
| :--------- | :---------------------------------------------- |
| `Mod+/`    | Set layout to tiles (horizontal + vertical)     |
| `Mod+,`    | Set layout to accordion (horizontal + vertical) |

### Window Navigation

#### Focusing Windows

| Keybinding | Action      |
| :--------- | :---------- |
| `Mod+H`    | Focus left  |
| `Mod+J`    | Focus down  |
| `Mod+K`    | Focus up    |
| `Mod+L`    | Focus right |

#### Moving Windows

| Keybinding    | Action     |
| :------------ | :--------- |
| `Mod+Shift+H` | Move left  |
| `Mod+Shift+J` | Move down  |
| `Mod+Shift+K` | Move up    |
| `Mod+Shift+L` | Move right |

### Resizing

| Keybinding | Action         |
| :--------- | :------------- |
| `Mod+-`    | Shrink by 50px |
| `Mod+=`    | Grow by 50px   |

### Workspaces

| Keybinding        | Action                                        |
| :---------------- | :-------------------------------------------- |
| `Mod+[1-9]`       | Switch to workspace 1-9                       |
| `Mod+Shift+[1-9]` | Move focused window to workspace 1-9          |
| `Mod+Tab`         | Switch back and forth between workspaces      |
| `Mod+Shift+Tab`   | Move workspace to next monitor (wraps around) |

### Modes

| Keybinding    | Action             |
| :------------ | :----------------- |
| `Mod+Shift+;` | Enter service mode |

## Service Mode

This mode is activated by pressing `Mod+Shift+;`. While in this mode, the following keybindings are active.

| Keybinding    | Action                                                |
| :------------ | :---------------------------------------------------- |
| `Esc`         | Reload config and return to main mode                 |
| `R`           | Flatten workspace tree and return to main mode        |
| `F`           | Toggle floating/tiling layout and return to main mode |
| `Backspace`   | Close all windows but current and return to main mode |
| `Mod+Shift+H` | Join with left and return to main mode                |
| `Mod+Shift+J` | Join with down and return to main mode                |
| `Mod+Shift+K` | Join with up and return to main mode                  |
| `Mod+Shift+L` | Join with right and return to main mode               |
