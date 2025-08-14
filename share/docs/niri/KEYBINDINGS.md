# Niri Keybindings

This document summarizes the keybindings configured for the Niri window manager.

The modifier key is `Mod4` (the "Windows" or "Super" key), referred to as `Mod` in this document.

## Default Mode

These keybindings are available in the default mode.

### Application Launchers

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Mod+T`                 | Launch terminal                                  |
| `Mod+d`                 | Launch application launcher (Fuzzel)             |

### System & Window Management

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Mod+Shift+Slash`       | Show hotkey overlay                              |
| `Mod+Q`                 | Close the focused window                         |
| `Mod+O`                 | Toggle overview mode                             |
| `Mod+Escape`            | Toggle keyboard shortcuts inhibit               |
| `Mod+Shift+E`           | Quit Niri                                        |
| `Ctrl+Alt+Delete`       | Quit Niri                                        |
| `Mod+Shift+P`           | Power off monitors                               |

### Window Focus Navigation

| Keybinding              | Action                        |
| ----------------------- | ----------------------------- |
| `Mod+h` / `Mod+Left`    | Focus column to the left      |
| `Mod+j` / `Mod+Down`    | Focus window below            |
| `Mod+k` / `Mod+Up`      | Focus window above            |
| `Mod+l` / `Mod+Right`   | Focus column to the right     |
| `Mod+Home`              | Focus first column            |
| `Mod+End`               | Focus last column             |

### Window Movement

| Keybinding                      | Action                                |
| ------------------------------- | ------------------------------------- |
| `Mod+Ctrl+h` / `Mod+Ctrl+Left`  | Move column to the left               |
| `Mod+Ctrl+j` / `Mod+Ctrl+Down`  | Move window down within column        |
| `Mod+Ctrl+k` / `Mod+Ctrl+Up`    | Move window up within column          |
| `Mod+Ctrl+l` / `Mod+Ctrl+Right` | Move column to the right              |
| `Mod+Ctrl+Home`                 | Move column to first position         |
| `Mod+Ctrl+End`                  | Move column to last position          |

### Monitor Management

#### Monitor Focus

| Keybinding                      | Action                        |
| ------------------------------- | ----------------------------- |
| `Mod+Shift+h` / `Mod+Shift+Left`| Focus monitor to the left     |
| `Mod+Shift+j` / `Mod+Shift+Down`| Focus monitor below           |
| `Mod+Shift+k` / `Mod+Shift+Up`  | Focus monitor above           |
| `Mod+Shift+l` / `Mod+Shift+Right`| Focus monitor to the right   |

#### Move Column to Monitor

| Keybinding                              | Action                               |
| --------------------------------------- | ------------------------------------ |
| `Mod+Shift+Ctrl+h` / `Mod+Shift+Ctrl+Left`  | Move column to monitor on the left   |
| `Mod+Shift+Ctrl+j` / `Mod+Shift+Ctrl+Down`  | Move column to monitor below         |
| `Mod+Shift+Ctrl+k` / `Mod+Shift+Ctrl+Up`    | Move column to monitor above         |
| `Mod+Shift+Ctrl+l` / `Mod+Shift+Ctrl+Right` | Move column to monitor on the right  |

### Workspace Management

#### Workspace Navigation

| Keybinding              | Action                        |
| ----------------------- | ----------------------------- |
| `Mod+[1-9]`             | Switch to workspace 1-9       |
| `Mod+u` / `Mod+Page_Down` | Focus workspace down         |
| `Mod+i` / `Mod+Page_Up` | Focus workspace up           |

#### Move Column to Workspace

| Keybinding                      | Action                                |
| ------------------------------- | ------------------------------------- |
| `Mod+Ctrl+[1-9]`                | Move column to workspace 1-9          |
| `Mod+Ctrl+u` / `Mod+Ctrl+Page_Down` | Move column to workspace down     |
| `Mod+Ctrl+i` / `Mod+Ctrl+Page_Up`   | Move column to workspace up       |

#### Move Workspace

| Keybinding                      | Action                        |
| ------------------------------- | ----------------------------- |
| `Mod+Shift+u` / `Mod+Shift+Page_Down` | Move workspace down    |
| `Mod+Shift+i` / `Mod+Shift+Page_Up`   | Move workspace up      |

### Column & Window Layout

#### Column Management

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Mod+BracketLeft`       | Consume or expel window to the left              |
| `Mod+BracketRight`      | Consume or expel window to the right             |
| `Mod+Comma`             | Consume window into column                       |
| `Mod+Period`            | Expel window from column                         |
| `Mod+W`                 | Toggle column tabbed display                     |

#### Window & Column Sizing

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Mod+R`                 | Switch preset column width                       |
| `Mod+Shift+R`           | Switch preset window height                      |
| `Mod+Ctrl+R`            | Reset window height                              |
| `Mod+F`                 | Maximize column                                  |
| `Mod+Shift+F`           | Fullscreen window                                |
| `Mod+Ctrl+F`            | Expand column to available width                 |
| `Mod+C`                 | Center column                                    |
| `Mod+Ctrl+C`            | Center visible columns                           |

#### Manual Size Adjustments

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Mod+Minus`             | Decrease column width by 10%                    |
| `Mod+Equal`             | Increase column width by 10%                    |
| `Mod+Shift+Minus`       | Decrease window height by 10%                   |
| `Mod+Shift+Equal`       | Increase window height by 10%                   |

### Floating Windows

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Mod+V`                 | Toggle window floating mode                      |
| `Mod+Shift+V`           | Switch focus between floating and tiling windows |

### Screenshots

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `Print`                 | Take screenshot (region selection)               |
| `Ctrl+Print`            | Take full screen screenshot                      |
| `Alt+Print`             | Take current window screenshot                   |

### Media & Hardware Controls

| Keybinding              | Action                                           |
| ----------------------- | ------------------------------------------------ |
| `XF86AudioRaiseVolume`  | Increase system volume                           |
| `XF86AudioLowerVolume`  | Decrease system volume                           |
| `XF86AudioMute`         | Toggle system volume mute                       |
| `XF86AudioMicMute`      | Toggle microphone mute                          |
| `XF86MonBrightnessUp`   | Increase screen brightness                       |
| `XF86MonBrightnessDown` | Decrease screen brightness                       |

## Key Concepts

Niri uses a **column-based layout** where windows are organized in vertical columns. Understanding this concept is essential for effective navigation:

- **Columns**: Vertical stacks that can contain one or more windows
- **Windows**: Individual applications within a column
- **Focus**: Move between columns (left/right) or windows within a column (up/down)
- **Movement**: Move entire columns or individual windows within columns
- **Workspaces**: Virtual desktops that can be arranged vertically
