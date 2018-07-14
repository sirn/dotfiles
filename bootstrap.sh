#!/usr/bin/env sh

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
    echo "    -p        Profile to install (default: asdf console desktop services)."
    echo
    echo "Available profiles:"
    echo
    echo "    asdf      Setup asdf-vm packages (e.g. language runtimes)."
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
            asdf | console | desktop | services ) echo "$1"; shift;;
            * ) break;;
        esac
    done | sort -u | awk 'FNR != 1 { printf " " } { printf }'
}

# shellcheck disable=SC2086
_profile=$(clean_profile $_profile)

if [ "$_profile" = "" ]; then
    _profile="asdf console desktop services"
fi

mkdir -p "$(dirname "$_config_file")"

cat <<-EOF > "$_config_file"
_profile="$_profile"
EOF


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
    printf "* %s\\n" "$1"
}

echo_error() {
    printf "\\033[0;31m!\\033[0;0m %s\\n" "$1"
}


#
# Common
#

common_ansible_run() {
    _playbook="_provision/playbook.yml"
    _ansible_config="_provision/ansible.cfg"
    _opts="-K -i _provision/hosts"

    if [ "$_ansible_python" != "" ]; then
        _opts="$_opts -e ansible_python_interpreter=$_ansible_python"
    fi

    if [ "$*" != "" ]; then
        _opts="$_opts $*"
    fi

    _skip_asdf=1
    _skip_console=1
    _skip_desktop=1
    _skip_services=1

    for p in $_profile; do
        case "$p" in
            asdf )     _skip_asdf=0;;
            console )  _skip_console=0;;
            desktop )  _skip_desktop=0;;
            services ) _skip_services=0;;
        esac
    done

    [ $_skip_asdf     = 1 ] && _opts="$_opts --skip-tags=asdf"
    [ $_skip_console  = 1 ] && _opts="$_opts --skip-tags=console"
    [ $_skip_desktop  = 1 ] && _opts="$_opts --skip-tags=desktop"
    [ $_skip_services = 1 ] && _opts="$_opts --skip-tags=services"

    # shellcheck disable=SC2086
    if ! env ANSIBLE_CONFIG="$_ansible_config" ansible-playbook "$_playbook" $_opts; then
        echo_error 'Ansible playbook exited with an error.'
        exit 1
    fi
}


#
# Darwin
#

bootstrap_darwin() {
    export PATH=/usr/local/bin:$PATH
    export HOMEBREW_NO_COLOR=1
    export HOMEBREW_NO_EMOJI=1
    export HOMEBREW_NO_ANALYTICS=1

    xcode-select --install 2>/dev/null
    if ! hash brew 2>/dev/null; then
        echo_wait 'Homebrew is not installed. Installing...'
        _install="https://raw.githubusercontent.com/Homebrew/install/master/install"
        /usr/bin/ruby -e "$(curl -fsSL $_install)"
    fi

    _ansible_bootstrapped=0

    if ! hash ansible-playbook 2>/dev/null; then
        echo_wait 'Ansible is not installed. Installing...'
        brew install ansible python@3
        _ansible_bootstrapped=1
        _ansible_python=/usr/local/bin/python3
    fi

    common_ansible_run "$@"

    for p in $_profile; do
        case "$p" in
            desktop )
                if [ "$*" = "" ]; then
                    if ! hash mas 2>/dev/null; then
                        echo_wait 'MAS is not installed. Installing...'
                        brew install mas
                    fi

                    brew cu -a -y
                    mas upgrade
                fi
                ;;
        esac
    done

    if [ $_ansible_bootstrapped = 1 ]; then
        brew tap beeftornado/rmtree
        echo_wait 'Uninstalling a bootstrapped Ansible...'
        brew rmtree ansible
	brew uninstall python@3
    fi

    rm "$HOME"/.homebrew_analytics_user_uuid >/dev/null 2>&1
}


#
# Main
#

_platform=$(uname)

echo_wait "Running bootstrap with profile(s): $_profile"

if [ "$*" != "" ]; then
    echo_wait "Ansible will be run with extra args: $*"
fi

case $_platform in
    Darwin) bootstrap_darwin "$@";;
    *)
        echo_error "Could not start bootstrap script."
        echo_error "Unknown platform: $_platform."
        exit 1
        ;;
esac
