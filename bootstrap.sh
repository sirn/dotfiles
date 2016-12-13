#!/usr/bin/env bash

#
# Utils
#

echo_clear() {
    echo -ne "\033[1A\r\033[K"
}

echo_ok() {
    echo -e "\033[0;32m✓\033[0;0m $1"
}

echo_wait() {
    echo -e "* \033[0;33m$1\033[0;0m"
}

echo_error() {
    echo -e "\033[0;31m!\033[0;0m $1"
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
    fi
}

darwin_brew_setup() {
    if hash brew 2>/dev/null; then
        echo_ok 'Homebrew is already installed.'
    else
        echo_wait 'Homebrew is not installed. Installing...'
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
}

darwin_ansible_bootstrap() {
    if hash ansible-playbook 2>/dev/null; then
        echo_ok 'Ansible is already installed.'
    else
        echo_wait 'Ansible is not installed. Installing...'
        brew install ansible
        darwin_ansible_bootstrapped=1
    fi
}

darwin_ansible_run() {
    echo_wait 'Running Ansible playbook. This will take a while.'
    if ! ansible-playbook provision/local.yml -K -i provision/hosts; then
        echo_error 'Ansible playbook exited with an error.'
        exit 1
    fi
}

darwin_cask_update() {
    if brew cask >/dev/null 2>&1; then
        echo_wait 'Cask appears to be installed. Checking for updates...'
        outdated_casks=()

        while read -ra cask_info; do
            echo -ne "\r\033[K  Checking \033[1;30m${cask_info[0]}\033[0;0m..."
            installed_version=${cask_info[${#cask_info[@]}-1]}
            latest_version=$(brew cask _stanza version "${cask_info[0]}")
            if [[ "$installed_version" != "$latest_version" ]]; then
                outdated_casks+=(${cask_info[0]})
            fi
        done <<< "$(brew cask list --versions)"

        echo -ne "\r\n"
        echo_clear

        if [ ${#outdated_casks[@]} == 0 ]; then
            echo_ok 'Casks are up-to-date.'
        else
            echo_wait "Updating casks: \033[1;30m${outdated_casks[*]}"
            brew cask uninstall --force "${outdated_casks[@]}"
            brew cask install "${outdated_casks[@]}"
        fi
    fi
}

darwin_cleanup() {
    echo_wait 'Cleaning up...'
    rm "$HOME"/.homebrew_analytics_user_uuid >/dev/null 2>&1
    brew cleanup
    brew cask cleanup

    if [[ $darwin_ansible_bootstrapped == 1 ]]; then
        echo_wait 'Uninstalling a bootstrapped Ansible...'
        brew uninstall ansible
    fi
}

bootstrap_darwin() {
    darwin_setenv
    darwin_xcode_setup
    darwin_brew_setup
    darwin_ansible_bootstrap
    darwin_ansible_run
    darwin_cask_update
    darwin_cleanup
}


#
# Main
#

case $OSTYPE in
    darwin*) bootstrap_darwin ;;
    *)
        echo_error "Could not start bootstrap script."
        echo_error "Unknown platform: $OSTYPE."
        exit 1
        ;;
esac
