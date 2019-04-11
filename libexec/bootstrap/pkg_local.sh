#!/bin/sh -e
#
# Install a package manually and locally.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


## Tmp
##

build_dir=$(mktemp -d)
if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $build_dir' 0 1 2 3 6 14 15
fi


## Utils
##

# See also: https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
_prepare_wxallowed() {
    name=$1; shift
    dest=$1; shift

    case $platform in
        openbsd )
            wxdir=/usr/local/$name

            printe_h2 "Preparing wxallowed for $name..."

            if file_absent "$wxdir" 1; then
                run_root mkdir -p "$wxdir"
                run_root chown "$USER:wheel" "$wxdir"
            fi

            make_link "$wxdir" "$dest"
            ;;
        * )
            ;;
    esac
}


## Rust
##

_prepare_wxallowed cargo "$HOME/.cargo"

printe_h2 "Installing rust..."

case $platform in
    openbsd )
        if ! command -v cargo >/dev/null; then
            printe "Rustup is not available under OpenBSD"
            printe "Try \`pkg_add rust\`"
        fi
        ;;

    * )
        PATH=$HOME/.cargo/bin:$PATH

        if file_absent "$HOME/.cargo/bin/rustup"; then
            fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
        fi
        ;;
esac

if command -v rustc >/dev/null; then
    rust_ver=$(rustc --version | awk '{ print $2 }')
    rust_src_base=$HOME/.local/lib/rustlib/src/rust

    if file_absent "$rust_src_base/rust-$rust_ver"; then
        mkdir -p "$rust_src_base"
        fetch_gh_archive - "rust-lang/rust" "$rust_ver" | tar -C "$rust_src_base" -xzf -
    fi

    make_link "$rust_src_base/rust-$rust_ver/src" "$rust_src_base/src"
fi

rust_pkglist=../../var/bootstrap/pkglist_rust.txt

if command -v cargo >/dev/null; then
    for f in $(mangle_file $rust_pkglist "$platform" "$flavors"); do
        printe_h2 "Installing rust packages from ${f##../../}..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            bin=${line%%:*}
            install=${line##$bin:}

            if file_absent "$HOME/.cargo/bin/$bin"; then
                # shellcheck disable=SC2086
                cargo install $install
            fi
        done < "$f"
    done
fi


## Node
##

case $platform in
    darwin )
        PATH=/usr/local/opt/node@10/bin:$PATH
        ;;

    * )
        ;;
esac

node_pkglist=../../var/bootstrap/pkglist_node.txt

if command -v npm >/dev/null; then
   npm set prefix="$HOME/.local"

   for f in $(mangle_file $node_pkglist "$platform" "$flavors"); do
       printe_h2 "Installing node packages from ${f##../../}..."
       xargs npm install -g < "$f"
   done
fi


## Haskell
##

haskell_pkglist=../../var/bootstrap/pkglist_haskell.txt

_prepare_wxallowed cabal "$HOME/.cabal"

if command -v cabal >/dev/null; then
    case "$platform" in
        openbsd )
            mkdir -p "$HOME/.cabal/build"

            _haskell_cabal() {
                env TMPDIR=/usr/local/cabal/build cabal "$@"
            }
            ;;

        * )
            _haskell_cabal() {
                cabal "$@"
            }
            ;;
    esac

    # Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
    # See https://github.com/haskell/cabal/blob/master/cabal-install/changelog
    haskell_cabal_prefix=

    if version_gte 2.4.0.0 "$(cabal --numeric-version)"; then
        haskell_cabal_prefix=v1-
    fi

    if file_absent "$HOME/.cabal/packages/hackage.haskell.org"; then
        printe_h2 "Updating haskell cabal package index..."
        _haskell_cabal "${haskell_cabal_prefix}update"
    fi

    for f in $(mangle_file $haskell_pkglist "$platform" "$flavors"); do
        printe_h2 "Installing haskell cabal packages from ${f##../../}..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            bin=${line%%:*}
            install=${line##$bin:}

            if file_absent "$HOME/.cabal/bin/$bin"; then
                # shellcheck disable=SC2086
                _haskell_cabal "${haskell_cabal_prefix}install" $install
            fi
        done < "$f"
    done
fi


## Extra packages
## These are packages that I use but not available under $platform
##

execline_ver=2.5.1.0
skalibs_ver=2.8.0.1
git_crypt_ver=0.6.0

case $platform in
    openbsd )
        ## gcloud
        ##

        printe_h2 "Creating Google Cloud state directory..."

        if file_absent "/usr/local/google-cloud-sdk/.install"; then
            run_root mkdir -p /usr/local/google-cloud-sdk/.install
        fi

        ## execline
        ##

        printe_h2 "Installing execline..."

        require_gmake "execline"

        if file_absent "$HOME/.local/lib/skalibs"; then
            fetch_gh_archive - skarnet/skalibs "v$skalibs_ver" | tar -C "$build_dir" -xzf -
            cd "$build_dir/skalibs-$skalibs_ver" || exit 1
            ./configure \
                --disable-shared \
                --prefix="$HOME/.local" \
                --libdir="$HOME/.local/lib"
            gmake install
            cd "$base_dir" || exit 1
        fi

        if file_absent "$HOME/.local/bin/execlineb"; then
            fetch_gh_archive - skarnet/execline "v$execline_ver" | tar -C "$build_dir" -xzf -
            cd "$build_dir/execline-$execline_ver" || exit 1
            ./configure \
                --disable-shared \
                --prefix="$HOME/.local" \
                --with-include="$HOME/.local/include" \
                --with-lib="$HOME/.local/lib" \
                --with-sysdeps="$HOME/.local/lib/skalibs/sysdeps"
            gmake install
            cd "$base_dir" || exit 1
        fi

        ## git-crypt
        ##

        printe_h2 "Installing git-crypt..."

        if file_absent "$HOME/.local/bin/git-crypt"; then
            fetch_gh_archive - AGWA/git-crypt "$git_crypt_ver" | tar -C "$build_dir" -xzf -
            cd "$build_dir/git-crypt-${git_crypt_ver}" || exit 1
            make
            make install PREFIX="$HOME/.local"
            cd "$base_dir" || exit 1
        fi

        ## Leiningen
        ##

        printe_h2 "Installing leiningen..."

        if file_absent "$HOME/.local/bin/lein"; then
            fetch_gh_raw "$HOME/.local/bin/lein" technomancy/leiningen stable bin/lein
            chmod 0755 "$HOME/.local/bin/lein"
            printe "leiningen has been successfully installed"
        fi

        ;;

    * )
        ;;
esac


## Kubernetes
##

if has_args "kubernetes" "$flavors"; then

    case $platform in
        openbsd )
            ## Kubectl
            ## Using master until updated vendor is in release:
            ## https://github.com/kubernetes/kubernetes/tree/master/vendor
            ##

            printe_h2 "Installing kubectl..."
            kubectl_ver=master

            require_go "kubectl"

            if file_absent "$HOME/.local/bin/kubectl"; then
                fetch_gh_archive - kubernetes/kubernetes "$kubectl_ver" | tar -C "$build_dir" -xzf -
                mkdir -p "$build_dir/go/src/k8s.io"
                mv "$build_dir/kubernetes-$kubectl_ver" "$build_dir/go/src/k8s.io/kubernetes"

                cd "$build_dir/go/src/k8s.io/kubernetes/cmd/kubectl" || exit 1
                env GOPATH="$build_dir/go" \
                    PATH="$build_dir/go/bin:$PATH" \
                    go install .

                cp "$build_dir/go/bin/kubectl" "$HOME/.local/bin/kubectl"
                printe "kubectl has been successfully installed"

                cd "$base_dir" || exit 1
            fi

            ## Helm
            ##

            printe_h2 "Installing helm..."
            helm_ver=2.13.1

            require_go "helm"
            require_gmake "kubectl"

            if file_absent "$HOME/.local/bin/helm"; then
                fetch_gh_archive - helm/helm "v$helm_ver" | tar -C "$build_dir" -xzf -
                mkdir -p "$build_dir/go/src/k8s.io"
                mv "$build_dir/helm-$helm_ver" "$build_dir/go/src/k8s.io/helm"

                cd "$build_dir/go/src/k8s.io/helm"
                env GOPATH="$build_dir/go" \
                    PATH="$build_dir/go/bin:$PATH" \
                    gmake bootstrap build

                cp "$build_dir/go/src/k8s.io/helm/bin/helm" "$HOME/.local/bin/helm"
                cp "$build_dir/go/src/k8s.io/helm/bin/tiller" "$HOME/.local/bin/tiller"
                printe "kubectl has been successfully installed"

                cd "$base_dir" || exit 1
            fi

            ;;

        * )
            ;;
    esac

    ## Kubectx
    ##

    printe_h2 "Installing kubectx..."

    git_clone_update https://github.com/ahmetb/kubectx.git "$HOME/.local/src/kubectx"
    make_link "$HOME/.local/src/kubectx/kubectx" "$HOME/.local/bin/kubectx"
    make_link "$HOME/.local/src/kubectx/kubens" "$HOME/.local/bin/kubens"

    ## Kapitan
    ## <rant>I hope to never touch this again</rant>
    ##

    printe_h2 "Installing kapitan..."

    # Kapitan has a strict version lock and require updating other dependencies
    # every time Kapitan is updated. We're locking the version here to ensure
    # custom-built dependencies (e.g. py-cryptography) works.
    kapitan_ver=0.23.0
    cryptography_ver=2.6.1
    jsonnet_ver=0.12.1

    # py-cryptography doesn't work well under LibreSSL (e.g. OpenBSD) without
    # patching the CFFI source to disable some OpenSSL features.
    case $(openssl version | tr '[:upper:]' '[:lower:]') in
        libressl* )
            printe_info "Patching py-cryptography for libressl..."

            if "$HOME/.asdf/shims/python3" -c 'import cryptography' >/dev/null 2>&1; then
                printe "py-cryptography already installed"
            else
                fetch_gh_archive - "pyca/cryptography" "$cryptography_ver" | tar -C "$build_dir" -xzf -
                cd "$build_dir/cryptography-$cryptography_ver" || exit 1
                for filename in \
                    security/py-cryptography/patches/patch-src__cffi_src_openssl_ssl_py \
                    security/py-cryptography/patches/patch-src__cffi_src_openssl_x509_vfy_py; do
                    fetch_gh_raw - \
                            openbsd/ports \
                            6fb634bd079c63a17a8f3f0a00a4e119b91bc9ad \
                            $filename |
                        patch -p0
                done

                "$HOME/.asdf/shims/pip3" install .
                cd "$base_dir" || exit 1
            fi
            ;;

        * )
            "$HOME/.asdf/shims/pip3" install cryptography==$cryptography_ver
            ;;
    esac

    case $platform in
        freebsd | openbsd )
            printe_info "Patching jsonnet for $(uname)..."

            require_gmake "jsonnet"

            if "$HOME/.asdf/shims/python3" -c 'import _jsonnet' >/dev/null 2>&1; then
                printe "jsonnet already installed"
            else
                fetch_gh_archive - google/jsonnet "v$jsonnet_ver" | tar -C "$build_dir" -xzf -
                cd "$build_dir/jsonnet-$jsonnet_ver" || exit 1

                # py-jsonnet assumes make is GNU make
                sed "s/'make'/'gmake'/g" < setup.py > setup.py.new
                mv setup.py.new setup.py

                # Jsonnet also assumes od is GNU-compatible
                od_bin="od"
                if [ "$platform" = "openbsd" ]; then
                    require_coreutils "jsonnet"
                    od_bin=ggod
                fi

                env \
                    OD=$od_bin \
                    CC=cc \
                    CXX=c++ \
                    CXXFLAGS="-fPIC -Iinclude -Ithird_party/md5 -Ithird_party/json -std=c++11" \
                    "$HOME/.asdf/shims/pip3" install .
                cd "$base_dir" || exit 1
            fi
            ;;

        * )
            env \
                CXXFLAGS="-fPIC -Iinclude -Ithird_party/md5 -Ithird_party/json -std=c++11" \
                "$HOME/.asdf/shims/pip3" install jsonnet==$jsonnet_ver
    esac

    env "$HOME/.asdf/shims/pip3" install kapitan==$kapitan_ver
    "$HOME/.asdf/bin/asdf" reshim python
fi
