#!/bin/sh -e
#
# Install Terraform.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

TF_VER=0.12.5
TF_SHA256=27dbd8e42b527f649a91df16b5c0ab4b26854eaa234a33514d0b6a6e09ce4c7d

_run() {
    printe_h2 "Installing terraform..."

    if ! forced && [ -f "$HOME/.local/bin/terraform" ]; then
        printe_info "$HOME/.local/bin/terraform already exists, skipping..."
        return
    fi

    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive terraform.tar.gz hashicorp/terraform v$TF_VER
    verify_shasum terraform.tar.gz $TF_SHA256
    tar -C "$BUILD_DIR" -xzf terraform.tar.gz
    rm terraform.tar.gz

    worksrc="$BUILD_DIR/go/src/github.com/hashicorp"
    mkdir -p "$worksrc"
    mv \
        "$BUILD_DIR/terraform-$TF_VER" \
        "$worksrc/terraform"

    make
    make tools
    mkdir -p "$HOME/.local/bin"
    cp "$worksrc/terraform/terraform" "$HOME/.local/bin/terraform"
    printe_info "terraform has been successfully installed"
}

_run
