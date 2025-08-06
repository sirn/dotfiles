# Emacs Keybindings Summary

This document provides a summary of the custom keybindings configured in this Emacs setup. It is organized into Global Keybindings and Leader Keybindings.

## Global Keybindings

These keybindings are generally available in most modes.

| Key | Command | Description |
| :--- | :--- | :--- |
| `C-x o` | `ace-window` | Select a window to switch to using `ace-window`. |
| `C-x C-b` | `consult-buffer` | Switch buffers using `consult`. |
| `C-x C-f` | `project-find-file` | Find file in the current project. |
| `C-x C-j` | `dired-jump` | Open directory of current buffer in Dired. |
| `C-c C-d` | `helpful-at-point` | Show help for the symbol at point. |
| `M-%` | `vr/query-replace` | `visual-regexp` query replace. |
| `M-g g` | `consult-goto-line` | Go to a specific line number in the current buffer. |
| `M-g i` | `consult-imenu` | Search for definitions/sections in the current buffer. |
| `M-g I` | `consult-imenu-multi` | Search for definitions/sections across multiple buffers. |
| `M-s r` | `consult-ripgrep` | Search the project with `ripgrep` (same as `leader s r`). |
| `M-y` | `consult-yank-pop` | Select an item from the kill ring to yank. |

### macOS Specific Global Keybindings

These bindings are active only when running Emacs on macOS.

| Key | Command | Description |
| :--- | :--- | :--- |
| `s-v` | `yank` | Paste from system clipboard. |
| `s-c` | `evil-yank` | Copy to system clipboard. |
| `s-w` | `delete-window` | Close the current window. |
| `s-W` | `delete-frame` | Close the current Emacs frame (application window). |
| `s-n` | `make-frame` | Create a new Emacs frame (application window). |

---

## Leader Keybindings (`SPC`)

All custom commands are organized under the `<leader>` key, which is `SPC` (Spacebar). Prefixes are marked with `(Prefix)` and will show a `which-key` menu.

### `SPC b` - Buffer Management

Buffer operations and management commands.

| Key | Command | Description |
| :--- | :--- | :--- |
| `SPC b b` | `consult-buffer` | Find buffer. |
| `SPC b k` | `kill-buffer` | Kill the current buffer. |
| `SPC b R` | `gemacs--revert-all-buffers` | Revert all file-visiting buffers. |
| `SPC b S` | `gemacs--save-all-buffers` | Save all modified buffers. |

### `SPC f` - File Operations

File finding and navigation commands.

| Key | Command | Description |
| :--- | :--- | :--- |
| `SPC f f` | `consult-fd` | Find file using `fd`. |
| `SPC f F` | `find-file` | Find file. |
| `SPC f o` | `consult-outline` | Search for headings in the current buffer. |

### `SPC p` - Project Commands

Project-specific commands and operations.

| Key | Command | Description |
| :--- | :--- | :--- |
| `SPC p` | `(Prefix)` | Project-specific commands. |
| `SPC p p` | `project-switch-project` | Switch to another project. |
| `SPC p '` | `eat-project` | Open a terminal in the project root. |
| `SPC p b` | `consult-project-buffer` | Find a buffer belonging to the current project. |
| `SPC p d` | `project-dired` | Open project root in Dired. |
| `SPC p f` | `gemacs--project-fd` | Find file in project using `fd`. |
| `SPC p g` | `consult-grep` | Search project with `grep`. |
| `SPC p m` | `magit-project-status` | Open Magit status for the project. |
| `SPC p r` | `consult-ripgrep` | Search project with `ripgrep`. |
| `SPC p s` | `gemacs--project-sync` | Sync known projects from `pom`. |

### `SPC E` - Environment

Environment variable management commands.

| Key | Command | Description |
| :--- | :--- | :--- |
| `SPC E` | `(Prefix)` | `envrc` commands for managing environment variables. |

### `'` - Terminal

| Key | Command | Description |
| :--- | :--- | :--- |
| `' '` | `eat` | Open a new `eat` terminal. |
| `' e` | `eshell` | Open `eshell`. |

### `a` - AI / LLM

| Key | Command | Description |
| :--- | :--- | :--- |
| `a a` | `aidermacs-transient-menu` | Show the Aidermacs (Aider) menu. |
| `a g` | `gemacs--gptel-transient-menu` | Show the GPTel menu. |

### `A` - Applications

#### `A m` - Mail (Notmuch)

| Key | Command | Description |
| :--- | :--- | :--- |
| `A m c` | `notmuch-mua-new-mail` | Compose a new email. |
| `A m m` | `notmuch` | Open the Notmuch email client. |

#### `A w` - Web (w3m)

| Key | Command | Description |
| :--- | :--- | :--- |
| `A w W` | `w3m` | Open the `w3m` browser. |
| `A w w` | `w3m-goto-url` | Go to a URL in `w3m`. |
| `A w s` | `w3m-search` | Search the web with `w3m`. |

### `c` - Code

| Key | Command | Description |
| :--- | :--- | :--- |
| `c e` | `(Prefix)` | Embark commands. |
| `c e a` | `embark-act` | Show contextual actions for the item at point. |
| `c e e` | `embark-dwim` | Execute the default action for the item at point. |
| `c e b` | `embark-bindings` | Show all possible Embark actions for the current context. |
| `c f` | `(Prefix)` | Flycheck error-checking commands. |
| `c f f` | `flycheck-list-errors` | List all flycheck errors (main flycheck action). |
| `c l` | `(Prefix)` | Eglot (LSP) commands. |
| `c x` | `(Prefix)` | Xref (cross-reference) commands. |

### `e` - Edit

#### `e i` - Ispell

| Key | Command | Description |
| :--- | :--- | :--- |
| `e i b` | `ispell-buffer` | Spell-check the entire buffer. |
| `e i i` | `ispell` | Start spell-checking. |
| `e i r` | `ispell-region` | Spell-check the selected region. |
| `e i w` | `ispell-word` | Spell-check the word at point. |

#### `e u` - Undo

| Key | Command | Description |
| :--- | :--- | :--- |
| `e u` | `undo-tree-visualize` | Open the undo-tree visualizer. |

#### `e x` - eXpressions (Smartparens)

| Key | Command | Description |
| :--- | :--- | :--- |
| `e x (` | `sp-wrap-round` | Wrap selection in `()`. |
| `e x {` | `sp-wrap-curly` | Wrap selection in `{}`. |
| `e x <` | `sp-backward-slurp-sexp` | Slurp expression backward. |
| `e x >` | `sp-forward-slurp-sexp` | Slurp expression forward. |
| `e x d` | `sp-splice-sexp` | Unwrap expression (splice). |
| `e x k` | `sp-kill-whole-line` | Kill the current line. |

#### `e y` - Yasnippet

| Key | Command | Description |
| :--- | :--- | :--- |
| `e y n` | `yas-new-snippet` | Create a new snippet. |
| `e y s` | `yas-insert-snippet` | Insert a snippet. |
| `e y v` | `yas-visit-snippet-file` | Visit the file for the current snippet. |

### `g` - Git

| Key | Command | Description |
| :--- | :--- | :--- |
| `g` | `magit-project-status` | Open Magit status for the current project. |

### `h` - Help

| Key | Command | Description |
| :--- | :--- | :--- |
| `h` | `(Prefix)` | `helpful` and other help commands. |
| `h h` | `helpful-at-point` | Show help for symbol at point. |

### `o` - Org Mode

| Key | Command | Description |
| :--- | :--- | :--- |
| `o` | `(Prefix)` | Org mode commands (capture, agenda, etc.). |

### `s` - Search / Jump

| Key | Command | Description |
| :--- | :--- | :--- |
| `s s` | `consult-ripgrep` | Search project with `ripgrep` (main search). |
| `s g` | `consult-grep` | Search project with `grep`. |
| `s j` | `avy-goto-char` | Jump to a character on screen. |
| `s J` | `avy-goto-char-2` | Jump to a 2-character sequence on screen. |
| `s l` | `avy-goto-line` | Jump to a line on screen. |
| `s L` | `ace-link` | Jump to a link, URL, or file path at point. |

### `t` - Toggles

| Key | Command | Description |
| :--- | :--- | :--- |
| `t f` | `display-fill-column-indicator-mode` | Toggle the fill-column indicator. |
| `t l` | `display-line-numbers-mode` | Toggle line numbers. |
| `t o` | `origami-mode` | Toggle Origami code folding mode. |
| `t t` | `dired-sidebar-toggle-with-current-directory` | Toggle Dired sidebar at current directory. |
| `t T` | `dired-sidebar-toggle-sidebar` | Toggle Dired sidebar. |

### `w` - Windows

| Key | Command | Description |
| :--- | :--- | :--- |
| `w /` | `split-window-right` | Split window vertically. |
| `w -` | `split-window-below` | Split window horizontally. |
| `w =` | `balance-windows` | Balance window sizes. |
| `w b` | `consult-buffer-other-window` | Open buffer in other window. |
| `w B` | `consult-buffer-other-frame` | Open buffer in other frame. |
| `w d` | `delete-window` | Delete the current window. |
| `w D` | `delete-other-windows` | Delete all other windows. |
| `w R` | `redraw-display` | Redraw the display. |
| `w w` | `ace-window` | Select a window to switch to. |

### `z` - Code Folding (Origami)

| Key | Command | Description |
| :--- | :--- | :--- |
| `z z` | `origami-toggle-node` | Toggle fold at point (main folding action). |
| `z c` | `origami-close-node` | Close fold at point. |
| `z o` | `origami-open-node` | Open fold at point. |
| `z r` | `origami-open-all-nodes` | Open all folds in buffer. |
| `z m` | `origami-close-all-nodes` | Close all folds in buffer. |
| `z n` | `origami-next-fold` | Move to next fold. |
| `z p` | `origami-previous-fold` | Move to previous fold. |
