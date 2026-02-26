# Sway Keybindings

This document summarizes the keybindings configured for the Sway window manager.

The modifier key is `Mod4` (the "Windows" or "Super" key), referred to as `Mod` in this document.

## Default Mode

These keybindings are available in the default mode.

### Session & Application Management

| Keybinding           | Action                                                  |
| -------------------- | ------------------------------------------------------- |
| `Mod+Return`         | Launch terminal                                         |
| `Mod+d`              | Launch application launcher (Fuzzel)                    |
| `Mod+Shift+q`        | Kill the focused window/application                     |
| `Mod+Shift+b`        | Send SIGUSR1 signal to Waybar (hide/unhide)             |
| `Mod+Shift+c`        | Reload the Sway configuration file                      |
| `Mod+Shift+e`        | Show a confirmation dialog to exit Sway                 |
| `Mod+Alt+L`          | Manually trigger `swayidle` (e.g., to lock screen)      |
| `Mod+Alt+F10`        | Manually trigger `kanshi` (e.g., to rearrange displays) |

### Window Navigation & Manipulation

#### Focusing Windows

| Keybinding            | Action                    |
| --------------------- | ------------------------- |
| `Mod+h` / `Mod+Left`  | Focus window to the left  |
| `Mod+j` / `Mod+Down`  | Focus window below        |
| `Mod+k` / `Mod+Up`    | Focus window above        |
| `Mod+l` / `Mod+Right` | Focus window to the right |
| `Mod+a`               | Focus parent container    |

#### Moving Windows

| Keybinding                | Action                      |
| ------------------------- | --------------------------- |
| `Mod+Shift+h` / `Mod+Shift+Left`  | Move window to the left     |
| `Mod+Shift+j` / `Mod+Shift+Down`  | Move window down            |
| `Mod+Shift+k` / `Mod+Shift+Up`    | Move window up              |
| `Mod+Shift+l` / `Mod+Shift+Right` | Move window to the right    |

#### Mouse Bindings (on floating windows)

The following mouse interactions are available for floating windows:

| Interaction                         | Action                  |
| ----------------------------------- | ----------------------- |
| `Mod + Left Mouse Button Drag`      | Move the window         |
| `Mod + Right Mouse Button Drag`     | Resize the window       |
| `Left Mouse Button Drag` on title bar | Move the window         |

### Layouts

| Keybinding          | Action                                      |
| ------------------- | ------------------------------------------- |
| `Mod+b`             | Split container horizontally                |
| `Mod+v`             | Split container vertically                  |
| `Mod+e`             | Toggle between split and non-split layout   |
| `Mod+s`             | Set layout to stacking                      |
| `Mod+w`             | Set layout to tabbed                        |
| `Mod+f`             | Toggle fullscreen for the focused window    |
| `Mod+Shift+space`   | Toggle floating mode for the focused window |
| `Mod+Shift+grave`   | Toggle sticky mode for the focused window   |
| `Mod+space`         | Toggle focus between floating and tiled windows |

### Workspaces

| Keybinding          | Action                               |
| ------------------- | ------------------------------------ |
| `Mod+[0-9]`         | Switch to workspace 1-10             |
| `Mod+Shift+[0-9]`   | Move focused window to workspace 1-10 |

### Scratchpad

| Keybinding          | Action                                    |
| ------------------- | ----------------------------------------- |
| `Mod+Shift+minus`   | Move the focused window to the scratchpad |
| `Mod+minus`         | Show/hide the scratchpad window           |

### Screenshots

| Keybinding          | Action                                           |
| ------------------- | ------------------------------------------------ |
| `Print`             | Capture the entire screen and save to `~/Desktop` |
| `Shift+Print`       | Capture the entire screen and copy to clipboard  |
| `Alt+Print`         | Capture a selected region and save to `~/Desktop` |
| `Alt+Shift+Print`   | Capture a selected region and copy to clipboard  |

### Modes

| Keybinding          | Action              |
| ------------------- | ------------------- |
| `Mod+r`             | Enter "resize" mode |

## Resize Mode

This mode is activated by pressing `Mod+r`. While in this mode, the following keybindings are active.

| Keybinding          | Action                      |
| ------------------- | --------------------------- |
| `h` / `Left`        | Shrink window width by 10px |
| `j` / `Down`        | Grow window height by 10px  |
| `k` / `Up`          | Shrink window height by 10px|
| `l` / `Right`       | Grow window width by 10px   |
| `Return` / `Escape` | Exit resize mode and return to default |

## Waybar Mouse Bindings

The following mouse interactions are available on the Waybar panel.

| Component             | Interaction      | Action                               |
| --------------------- | ---------------- | ------------------------------------ |
| Workspaces            | Left-click button| Switch to the selected workspace     |
| Workspaces            | Scroll wheel     | Cycle through workspaces             |
| PulseAudio icon       | Left-click       | Launch PulseAudio Volume Control (`pavucontrol`) |
