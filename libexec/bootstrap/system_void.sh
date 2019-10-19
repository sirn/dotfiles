#!/bin/sh -e
#
# Sets up Void Linux system.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

_run() {
    printe_h2 "Setting up cronie..."

    if ! [ -d /etc/sv/cronie ]; then
        printe_info "cronie is not installed, skipping..."
        return
    fi

    if ! forced && [ -L /var/service/cronie ]; then
        printe_info "/var/service/cronie already exists, skipping..."
        return
    fi

    run_root ln -s /etc/sv/cronie /var/service/cronie
    printe "/var/service/cronie successfully installed"
}

run_with_flavors "$@"
