# Dotfiles

This repository hosts my personal dotfiles as well as machine provisioning and workflow scripts.

### Usage

Currently targetting **macOS** and **Arch Linux** (although Arch Linux is very much untested).

```
$ git clone https://github.com/sirn/dotfiles.git .dotfiles
$ cd dotfiles
$ ./bootstrap.sh
```

### Structure

```
.
├── _bin                  workflow scripts
├── _provision            ansible playbook
│   ├── library           ├─ ansible modules
│   └── roles             └─ ansible roles
│       ├── local            ├─ provisioning script for setting up dotfiles
│       └── packages         └─ provisioning script for setting up packages and services
├── amethyst              macos window manager
├── aria2                 a better downloader
├── emacs                 the one true os
├── git                   dvcs made by a crazy person
├── hg                    dvcs made by another crazy person
├── sh                    because I like compatibilities
├── ssh                   even though I have no idea about security
├── xmonad                the best window manager that I can't use with macos
├── xorg                  sometimes I use non-macos
└── zsh                   for my own sanity
```

### Point of interest

-   [**Any**: Use Ansible to manage asdf installations](_provision/roles/packages/tasks/lang/)
-   [**macOS**: Use Ansible to install macOS applications with Homebrew-Cask](_provision/roles/packages/tasks/packages/darwin/cask.yml)
-   [**macOS**: Use Ansible to install Mac App Store applications with MAS](_provision/roles/packages/tasks/packages/darwin/mas.yml)
-   [**macOS**: Check Sparkle settings for all installed apps](_bin/check-sparkle)
-   [**macOS**: Using funtoo's keychain for ssh-agent/gpg-agent on macOS](_provision/roles/packages/tasks/services/darwin/env.yml)
-   [**Arch Linux**: pacaur module for Ansible](_provision/library/pacaur.py)

### License

MIT
