# Zellij Keybindings

This document summarizes the custom keybindings configured for Zellij.

The default mode is `locked`, and most keybindings switch between modes.

## Global Keybindings (Normal & Locked Modes)

| Key | Action |
|:----|:-------|
| `Alt+←/h` | Move focus or tab left |
| `Alt+↓/j` | Move focus down |
| `Alt+↑/k` | Move focus up |
| `Alt+→/l` | Move focus or tab right |
| `Alt++/=` | Increase pane size |
| `Alt+-` | Decrease pane size |
| `Alt+[` | Previous swap layout |
| `Alt+]` | Next swap layout |
| `Alt+i` | Move tab left |
| `Alt+o` | Move tab right |

## Mode Switching

| Key | Action |
|:----|:-------|
| `Ctrl+q` | Switch to locked mode |
| `Enter` | Switch to locked mode |
| `Esc` | Switch to locked mode |

## Locked Mode

| Key | Action |
|:----|:-------|
| `Ctrl+g` | Switch from locked mode |

## Pane Mode (`p`)

| Key | Action |
|:----|:-------|
| `←/↓/↑/→` | Move focus in direction |
| `h/j/k/l` | Move focus (vim-style) |
| `c` | Rename pane |
| `d` | New pane down |
| `e` | Toggle pane embed/floating |
| `f` | Toggle focus fullscreen |
| `i` | Toggle pane pinned |
| `n` | New pane |
| `r` | New pane right |
| `w` | Toggle floating panes |
| `x` | Close focused pane |
| `z` | Toggle pane frames |
| `Tab` | Switch focus |

## Tab Mode (`t`)

| Key | Action |
|:----|:-------|
| `←/↓/↑/→` | Navigate tabs |
| `h/j/k/l` | Navigate tabs (vim-style) |
| `1-9` | Go to tab number |
| `[` | Break pane left |
| `]` | Break pane right |
| `b` | Break pane |
| `n` | New tab |
| `r` | Rename tab |
| `s` | Toggle active sync tab |
| `x` | Close tab |
| `Tab` | Toggle tab |

## Resize Mode (`r`)

| Key | Action |
|:----|:-------|
| `←/↓/↑/→` | Increase resize in direction |
| `h/j/k/l` | Increase resize (vim-style) |
| `H/J/K/L` | Decrease resize (vim-style) |
| `+/=` | Increase size |
| `-` | Decrease size |

## Move Mode (`m`)

| Key | Action |
|:----|:-------|
| `←/↓/↑/→` | Move pane in direction |
| `h/j/k/l` | Move pane (vim-style) |
| `n` | Move pane forward |
| `p` | Move pane backwards |
| `Tab` | Move pane |

## Scroll Mode (`s`)

| Key | Action |
|:----|:-------|
| `Alt+←/↓/↑/→` | Move focus or tab and exit |
| `Alt+h/j/k/l` | Move focus or tab and exit (vim-style) |
| `e` | Edit scrollback |
| `f` | Enter search mode |

### Scroll Navigation

| Key | Action |
|:----|:-------|
| `←/h` | Page scroll up |
| `↓/j` | Scroll down |
| `↑/k` | Scroll up |
| `→/l` | Page scroll down |
| `PageUp` | Page scroll up |
| `PageDown` | Page scroll down |
| `Ctrl+b` | Page scroll up |
| `Ctrl+f` | Page scroll down |
| `Ctrl+c` | Scroll to bottom and exit |
| `d` | Half page scroll down |
| `u` | Half page scroll up |

## Search Mode

| Key | Action |
|:----|:-------|
| `c` | Toggle case sensitivity |
| `n` | Search down |
| `o` | Toggle whole word |
| `p` | Search up |
| `w` | Toggle wrap |

## Session Mode (`o`)

| Key | Action |
|:----|:-------|
| `a` | Launch about plugin |
| `c` | Launch configuration plugin |
| `d` | Detach session |
| `p` | Launch plugin manager |
| `q` | Quit Zellij |
| `w` | Launch session manager |

## Normal Mode

| Key | Action |
|:----|:-------|
| `f` | Toggle floating panes |
