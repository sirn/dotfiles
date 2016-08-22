#!/usr/bin/env bash

#
# Xcode Command-Line Tools
#

echo -e "* \033[0;33mDetermining Xcode status. You may be asked to enter root password.\033[0;0m"
xcode-select --install 2>/dev/null
if [[ $? == 1 ]] ; then
    echo -e "\033[1A\r\033[K* Xcode has been successfully setup. \033[0;32m✓\033[0;0m"
fi


#
# Homebrew
#

which -s brew
if [[ $? != 0 ]] ; then
    echo -e "* \033[0;33mHomebrew is not installed. Installing.\033[0;0m"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
    echo -e "* Homebrew is already installed. \033[0;32m✓\033[0;0m"
fi


#
# Ansible
#

which -s ansible-playbook
if [[ $? != 0 ]] ; then
    export ansible_bootstrapped=1
    echo -e "* \033[0;33mAnsible is not installed. Installing.\033[0;0m"
    $(which brew) install ansible
else
    echo -e "* Ansible is already installed. \033[0;32m✓\033[0;0m"
fi


#
# Playbook
#

echo -e "* \033[0;33mRunning Ansible playbook. This will take a while.\033[0;0m"
$(which ansible-playbook) provision/local.yml -K -i provision/hosts "$@"


#
# Brew Cask
#

brew cask >/dev/null 2>&1
if [[ $? == 0 ]] ; then
    echo -e "* \033[0;33mCask appears to be installed. Checking for updates.\033[0;0m"
    OUTDATED_CASKS=()

    while read -ra CASK_INFO; do
        echo -ne "\r\033[K  Checking \033[1;30m${CASK_INFO[0]}\033[0;0m..."
        INSTALLED_VERSION=${CASK_INFO[${#CASK_INFO[@]}-1]}
        LATEST_VERSION=$(brew cask _stanza version "${CASK_INFO[0]}")
        if [[ "$INSTALLED_VERSION" != "$LATEST_VERSION" ]]; then
            OUTDATED_CASKS+=(${CASK_INFO[0]})
        fi
    done <<< "$(brew cask list --versions)"

    if [ ${#OUTDATED_CASKS[@]} != 0 ]; then
        echo -e "\r\033[K\033[1A\r\033[K* \033[0;33mUpdating casks: \033[1;30m${OUTDATED_CASKS[*]}\033[0;0m"
        brew cask install --force "${OUTDATED_CASKS[@]}"
    else
        echo -e "\r\033[K\033[1A\r\033[K* Casks are up-to-date. \033[0;32m✓\033[0;0m"
    fi
fi


#
# Cleanup
#

echo -e "* \033[0;33mCleaning up...\033[0;0m"
rm "$HOME"/.homebrew_analytics_user_uuid >/dev/null 2>&1
brew cleanup
brew cask cleanup

if [ $ansible_bootstrapped ]; then
    echo -e "* \033[0;33mUninstalling a bootstrapped Ansible.\033[0;0m"
    $(which brew) uninstall ansible
fi
