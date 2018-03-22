# Dotfiles

This repository hosts my personal dotfiles and machine provisioning and workflow scripts.

### Usage

Currently supporting **macOS** and **Arch Linux** (although Arch Linux is very much untested).

```
$ git clone https://github.com/sirn/dotfiles.git .dotfiles
$ cd dotfiles
$ ./bootstrap.sh
```

### Structure

```
.
├── bin                   workflow scripts
├── emacs                 emacs configurations
├── etc                   other configurations (unstructured)
├── provision             ansible playbook
│   ├── library           ├─ ansible modules
│   └── roles             └─ ansible roles
│       ├── local            ├─ provisioning script for setting up dotfiles
│       └── packages         └─ provisioning script for setting up packages and services
├── ssh                   ssh configurations
└── zsh                   zsh configurations
```

### Point of interest

* [**Any**: Use Ansible to manage asdf installations](provision/roles/packages/tasks/lang/)
* [**Any**: Sync repositories automatically with GHQ](bin/ghq-sync)
* [**macOS**: Use Ansible to install macOS applications with Homebrew-Cask](provision/roles/packages/tasks/packages/darwin/cask.yml)
* [**macOS**: Use Ansible to install Mac App Store applications with MAS](provision/roles/packages/tasks/packages/darwin/mas.yml)
* [**macOS**: Check Sparkle settings for all installed apps](bin/check-sparkle)
* [**macOS**: Using funtoo's keychain for ssh-agent/gpg-agent on macOS](provision/roles/packages/tasks/services/darwin/env.yml)
* [**macOS**: Using unbound to create .dev TLD for local development use](provision/roles/packages/tasks/services/darwin/unbound.yml)
* [**Arch Linux**: using unbound to create .dev TLD for local development use](provision/roles/packages/tasks/services/archlinux/unbound.yml)
* [**Arch Linux**: pacaur module for Ansible](provision/library/pacaur.py)

### License

MIT
