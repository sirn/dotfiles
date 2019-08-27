#!/bin/sh -e
#
# Install Terraform.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

TF_VER=0.12.7
TF_SHA256=a0fa11217325f76bf1b4f53b0f7a6efb1be1826826ef8024f2f45e60187925e7

_run() {
    printe_h2 "Installing terraform (bin)..."

    if ! forced && [ -f "$HOME/.local/bin/terraform" ]; then
        printe_info "$HOME/.local/bin/terraform already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    if command -v arch >/dev/null; then
        arch=$(arch)
    elif ! arch=$(sysctl -n hw.machine_arch); then
        printe_info "Could not determine arch, skipping..."
        return
    fi

    case "$arch" in
        x86_64 | amd64 ) arch=amd64;;
        x86 | i386 )     arch=386;;
        aarch64 )        arch=arm;;
        * )
            printe_info "$arch is unsupported by Terraform, skipping..."
            return
            ;;
    esac

    platform=$(uname | tr "[:upper:]" "[:lower:]")
    distsite=https://releases.hashicorp.com/terraform/$TF_VER
    distfile=terraform_${TF_VER}_${platform}_$arch.zip

    fetch_url terraform.zip "$distsite/$distfile"
    verify_shasum terraform.zip $TF_SHA256
    unzip -d "$BUILD_DIR" terraform.zip
    rm terraform.zip

    mkdir -p "$HOME/.local/bin"
    cp "$BUILD_DIR/terraform" "$HOME/.local/bin/terraform"
    chmod +x "$HOME/.local/bin/terraform"
    printe_info "terraform has been successfully installed"
}

_run
