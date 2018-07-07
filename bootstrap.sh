#!/usr/bin/env bash

#
# Utils
#

echo_clear() {
    printf "\\033[1A\\r\\033[K"
}

echo_ok() {
    printf "\\033[0;32m✓\\033[0;0m %s\\n" "$1"
}

echo_wait() {
    printf "* \\033[0;33m%s\\033[0;0m\\n" "$1"
}

echo_error() {
    printf "\\033[0;31m!\\033[0;0m %s\\n" "$1"
}


#
# Common
#

common_ansible_run() {
    echo_wait 'Running Ansible playbook. This will take a while.'

    local _playbook="_provision/local.yml"
    local _config="_provision/ansible.cfg"
    local _opts=(-K -i _provision/hosts)

    if [[ $ansible_python != "" ]]; then
        _opts+=(-e "ansible_python_interpreter=${ansible_python}")
    fi

    if ! env ANSIBLE_CONFIG="$_config" ansible-playbook "$_playbook" "${_opts[@]}" "$@"; then
        echo_error 'Ansible playbook exited with an error.'
        exit 1
    fi
}


#
# Darwin
#

darwin_setenv() {
    export PATH=/usr/local/bin:$PATH
    darwin_ansible_bootstrapped=0
}

darwin_xcode_setup() {
    echo_wait 'Determining Xcode status. You may need to enter root password...'
    if ! xcode-select --install 2>/dev/null; then
        echo_clear
        echo_ok 'Xcode has been successfully setup.'
	echo_ok 'Please make sure to run sudo xcodebuild -license'
    fi
}

darwin_brew_setup() {
    if hash brew 2>/dev/null; then
        echo_ok 'Homebrew is already installed.'
    else
        echo_wait 'Homebrew is not installed. Installing...'
        local _install="https://raw.githubusercontent.com/Homebrew/install/master/install"
        /usr/bin/ruby -e "$(curl -fsSL $_install)"
    fi
}

darwin_ansible_bootstrap() {
    if hash ansible-playbook 2>/dev/null; then
        echo_ok 'Ansible is already installed.'
        if [ -e "$HOME/.asdf/shims/python3" ]; then
            ansible_python="$HOME/.asdf/shims/python3"
        fi
    else
        echo_wait 'Ansible is not installed. Installing...'
        brew install ansible python@2
        darwin_ansible_bootstrapped=1
        ansible_python=/usr/local/bin/python2
    fi
}

darwin_cask_update() {
    echo_wait 'Cask appears to be installed. Performing updates...'
    brew cu -a -y
}

darwin_mas_update() {
    if hash mas 2>/dev/null; then
        echo_ok 'MAS appears to be installed. Performing updates...'
    else
        echo_wait 'MAS is not installed. Installing...'
        brew install mas
    fi
    mas upgrade
}

darwin_cleanup() {
    echo_wait 'Cleaning up...'
    rm "$HOME"/.homebrew_analytics_user_uuid >/dev/null 2>&1
    brew cleanup
    brew cask cleanup

    if [[ $darwin_ansible_bootstrapped == 1 ]]; then
        brew tap beeftornado/rmtree
        echo_wait 'Uninstalling a bootstrapped Ansible...'
        brew rmtree ansible
	brew uninstall python@2
    fi
}

bootstrap_darwin() {
    darwin_setenv
    darwin_xcode_setup
    darwin_brew_setup
    darwin_ansible_bootstrap
    common_ansible_run "$@"

    if [[ "$*" == "" ]]; then
        darwin_cask_update
        darwin_mas_update
    fi

    darwin_cleanup
}


#
# Linux
#

linux_arch_setenv() {
    export PATH=/usr/local/bin:$PATH
}

linux_arch_ansible_bootstrap() {
    if hash ansible-playbook 2>/dev/null; then
        echo_ok 'Ansible is already installed.'
    else
        echo_wait 'Ansible is not installed. Installing...'
        sudo pacman -S --noconfirm ansible python
    fi
}

bootstrap_linux() {
    if hash pacman 2>/dev/null; then
        linux_arch_setenv
        linux_arch_ansible_bootstrap
        common_ansible_run "$@"
    else
        echo_error "No compatible package manager."
        exit 1
    fi
}


#
# Main
#

case $OSTYPE in
    darwin*) bootstrap_darwin "$@" ;;
    linux*)  bootstrap_linux "$@" ;;
    *)
        echo_error "Could not start bootstrap script."
        echo_error "Unknown platform: $OSTYPE."
        exit 1
        ;;
esac
