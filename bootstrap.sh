#!/bin/bash

#
# Xcode Command-Line Tools
#

echo -e "* \033[0;33mDetermining Xcode status. You may be asked to enter root password.\033[0;0m"
xcode-select --install 2>/dev/null
if [[ $? == 1 ]] ; then
    echo -e "\033[1A\r\033[K* Xcode has been successfully setup. \033[0;32m✓\033[0;0m"
fi


#
# Nix
#

which -s nix-env
if [[ $? != 0 ]] ; then
    echo -e "* \033[0;33mNix is not installed. Installing.\033[0;0m"
    curl https://nixos.org/nix/install | sh
    rmdir nix-binary-tarball-unpack >/dev/null 2>&1
    . /Users/sirn/.nix-profile/etc/profile.d/nix.sh
else
    echo -e "* Nix is already installed. \033[0;32m✓\033[0;0m"
fi


#
# Ansible
#

which -s ansible-playbook
if [[ $? != 0 ]] ; then
    echo -e "* \033[0;33mAnsible is not installed. Installing.\033[0;0m"
    $(which nix-env) -f '<nixpkgs>' -iA pythonPackages.ansible2
else
    echo -e "* Ansible is already installed. \033[0;32m✓\033[0;0m"
fi


#
# Playbook
#

echo -e "* \033[0;33mRunning Ansible playbook. This will take a while.\033[0;0m"
$(which ansible-playbook) provision/local.yml -K -i provision/hosts "$@"
