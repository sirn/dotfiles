#!/bin/sh -e
#
# Install postgres packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

POSTGRES_VERSION=14.1

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping postgres..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing postgres..."
    _install_postgres
}

_install_postgres() {
    case "$(uname)" in
        Darwin )
            POSTGRES_EXTRA_CONFIGURE_OPTIONS="\
--with-includes=/opt/local/include/openssl-1.1 \
--with-libraries=/opt/local/lib/openssl-1.1\
"
            export POSTGRES_EXTRA_CONFIGURE_OPTIONS
            ;;
    esac

    asdf_plugin postgres https://github.com/smashedtoatoms/asdf-postgres
    asdf_install postgres "$POSTGRES_VERSION" global
}

run_with_flavors "$@"
