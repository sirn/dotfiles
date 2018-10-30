#!/bin/sh

#
# Self-bootstrapping
# Yo dawg
#

_repo_tar="https://github.com/sirn/dotfiles/archive/master.tar.gz"
_repo_ssh="git@github.com:sirn/dotfiles.git"
_dotfiles="$HOME/.dotfiles"

_platform=$(uname)

self_bootstrap_darwin() {
    curl -sL "$_repo_tar" | tar -xvzf - --strip-components=1 -C "$_dotfiles"
}

self_bootstrap_freebsd() {
    fetch -o - "$_repo_tar" | tar -xvzf - --strip-components=1 -C "$_dotfiles"
}

if [ ! -f "$_dotfiles/bootstrap.sh" ]; then
    mkdir -p "$_dotfiles"

    case $_platform in
        Darwin)  self_bootstrap_darwin;;
        FreeBSD) self_bootstrap_freebsd;;
        *)
            echo "Unknown platform: $_platform."
            exit 1
            ;;
    esac

    # shellcheck disable=SC1090
    exec "$_dotfiles/bootstrap.sh" "$@"
    exit 0
fi


#
# Includes
#

# shellcheck source=_share/ansible.sh
. "$HOME/.dotfiles/_share/ansible.sh"

# shellcheck source=_share/utils.sh
. "$HOME/.dotfiles/_share/utils.sh"


#
# Configurations
#

if [ "$XDG_CONFIG_HOME" = "" ]; then
    XDG_CONFIG_HOME="$HOME/.config"
fi

_config_file="$XDG_CONFIG_HOME/dotfiles"

print_usage() {
    echo "Usage: $0 [-h] [-r] [[-p profile1] [-p profile2] ...] [-- [ansible args...]]"
    echo
    echo "    -h        Print this help."
    echo "    -r        Reset configuration."
    echo "    -p        Profile to install (default: console desktop services)."
    echo
    echo "Available profiles:"
    echo
    echo "    console   Setup console packages and profiles."
    echo "    desktop   Setup desktop packages."
    echo "    services  Setup system services."
    echo
}

reset_config() {
    _profile=""
    if [ -f "$_config_file" ]; then
        rm "$_config_file"
    fi
}

_profile=""
_first_run="1"

# shellcheck disable=SC1090
if [ -f "$_config_file" ]; then
    . "$_config_file"
fi


#
# Args parsing
#

ARGS=$(getopt hrp: "$@")

# shellcheck disable=SC2181
if [ $? != 0 ]; then
    print_usage
    exit 1
fi

eval set -- "$ARGS"

while true; do
    case "$1" in
        -h ) print_usage; exit 2;;
        -r ) reset_config; shift;;
        -p ) _profile="$_profile $2"; shift; shift;;
        -- ) shift; break;;
        *  ) break;;
    esac
done


#
# Profile handling
#

clean_profile() {
    while true; do
        case "$1" in
            console | desktop | services ) echo "$1"; shift;;
            * ) break;;
        esac
    done | sort -u | awk 'FNR != 1 { printf " " } { printf }'
}

# shellcheck disable=SC2086
_profile=$(clean_profile $_profile)

if [ "$_profile" = "" ]; then
    _profile="console desktop services"
fi

mkdir -p "$(dirname "$_config_file")"

cat <<-EOF > "$_config_file"
_profile="$_profile"
_first_run="0"
EOF


#
# Common
#

common_ansible_run() {
    _playbook="$_dotfiles/_provision/playbook.yml"
    _ansible_config="$_dotfiles/_provision/ansible.cfg"
    _opts="-i $_dotfiles/_provision/hosts"

    if [ "$_ansible_bin" = "" ]; then
        _ansible_bin=ansible-playbook
    fi

    if [ "$_ansible_opts" != "" ]; then
        _opts="$_opts $_ansible_opts"
    fi

    if [ "$_ansible_python" != "" ]; then
        _opts="$_opts -e ansible_python_interpreter=$_ansible_python"
    fi

    if [ "$*" != "" ]; then
        _opts="$_opts $*"
    fi

    _skip_console=1
    _skip_desktop=1
    _skip_services=1

    for p in $_profile; do
        case "$p" in
            console )  _skip_console=0;;
            desktop )  _skip_desktop=0;;
            services ) _skip_services=0;;
        esac
    done

    [ $_skip_console  = 1 ] && _opts="$_opts --skip-tags=console"
    [ $_skip_desktop  = 1 ] && _opts="$_opts --skip-tags=desktop"
    [ $_skip_services = 1 ] && _opts="$_opts --skip-tags=services"

    # shellcheck disable=SC2086
    if ! env ANSIBLE_CONFIG="$_ansible_config" "$_ansible_bin" "$_playbook" $_opts; then
        echo_error 'Ansible playbook exited with an error.'
        exit 1
    fi
}


#
# Darwin
#

bootstrap_darwin() {
    export PATH=$PATH:$HOME/.local/bin:/usr/local/bin
    export HOMEBREW_NO_EMOJI=1
    export HOMEBREW_NO_ANALYTICS=1

    xcode-select --install 2>/dev/null

    _sdk="/Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg"
    if [ -e "$_sdk" ] && [ ! -f /usr/lib/bundle1.o ]; then
        sudo /usr/sbin/installer -pkg "$_sdk" -target /
    fi

    if ! hash brew 2>/dev/null; then
        echo_wait 'Homebrew is not installed. Installing...'
        _install="https://raw.githubusercontent.com/Homebrew/install/master/install"
        /usr/bin/ruby -e "$(curl -fsSL $_install)"
    fi

    /usr/local/bin/brew update

    if [ ! -f "$_ansible_python" ]; then
        echo_wait 'Python is not installed. Installing...'
        /usr/local/bin/brew install python@3
    fi

    if [ ! -f "$_ansible_bin" ]; then
        echo_wait 'Ansible is not installed. Installing...'
        /usr/local/bin/brew install ansible
    fi

    common_ansible_run "$@"

    for p in $_profile; do
        case "$p" in
            desktop )
                if [ "$*" = "" ]; then
                    if ! hash mas 2>/dev/null; then
                        echo_wait 'MAS is not installed. Installing...'
                        /usr/local/bin/brew install mas
                    fi

                    /usr/local/bin/brew cask upgrade
                    /usr/local/bin/mas upgrade
                fi
                ;;
        esac
    done

    if [ "$*" = "" ]; then
        /usr/local/bin/brew upgrade
    fi

    # Some weird packages will randomly chmod /usr/local/Cellar
    if [ -d /usr/local/Cellar ]; then
        chmod ug+rwx,o+rx /usr/local/Cellar
    fi

    rm "$HOME"/.homebrew_analytics_user_uuid >/dev/null 2>&1
}


#
# FreeBSD
#

bootstrap_freebsd() {
    export PATH=$PATH:$HOME/.local/bin:/usr/local/bin
    export LANG=en_US.UTF-8

    # Perma-disable FreeBSD repo as we'll be exclusively using Synth.
    sudo mkdir -p /usr/local/etc/pkg/repos
    echo "FreeBSD: { enabled: no }" | sudo tee /usr/local/etc/pkg/repos/10_freebsd.conf >/dev/null

    if [ ! -d /usr/ports/ports-mgmt/synth ]; then
        sudo portsnap fetch --interactive
        sudo portsnap extract
    elif [ "$_first_run" = "1" ]; then
        sudo portsnap fetch --interactive
        sudo portsnap extract
        sudo portsnap update
    fi

    if ! hash synth 2>/dev/null; then
        echo_wait 'Synth is not installed. Installing...'
        sudo make -C /usr/ports/ports-mgmt/synth -DBATCH install clean
    fi

    if [ ! -f "$_ansible_python" ]; then
        echo_wait 'Python is not installed. Installing...'
        sudo /usr/local/bin/synth install security/ca_root_nss lang/python36
    fi

    if [ ! -f "$_ansible_bin" ]; then
        echo_wait 'Ansible is not installed. Installing...'
        sudo /usr/local/bin/synth install sysutils/ansible@py36
    fi

    common_ansible_run "$@"

    if [ "$*" = "" ]; then
        sudo /usr/local/bin/synth upgrade-system
    fi
}


#
# Main
#

echo_wait "Running bootstrap with profile(s): $_profile"

if [ "$*" != "" ]; then
    echo_wait "Ansible will run with extra args: $*"
fi

case $_platform in
    Darwin)  bootstrap_darwin "$@";;
    FreeBSD) bootstrap_freebsd "$@";;
    *)
        echo_error "Could not start bootstrap script."
        echo_error "Unknown platform: $_platform."
        exit 1
        ;;
esac


#
# Re-git
#

if [ ! -d "$_dotfiles/.git" ]; then
    echo "Once you have SSH keys setup, you might want to re-init dotfiles:"
    echo
    echo "  cd $_dotfiles"
    echo "  git init"
    echo "  git remote add origin $_repo_ssh"
    echo "  git fetch"
    echo "  git reset --hard origin/master"
    echo
fi
