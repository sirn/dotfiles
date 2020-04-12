#!/bin/sh -e
#
# Install execline.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../../share/bootstrap/utils.sh"
. "../../../share/bootstrap/buildenv.sh"

_install_font() {
    srcdir=$1; shift
    dest=$1; shift

    rm -rf "$dest" || exit 1
    mkdir -p "$dest" || exit 1

    find "$srcdir" \
         \( -iname "*.ttf" -or -iname "*.ttc" \) \
         -exec install -m0644 \{\} "$dest" \;

    touch "$dest/.installed"
}

_install_font_gh() {
    name=$1; shift
    repo=$1; shift
    shasum=$1; shift
    ver=$1; shift

    fontdir=${XDG_DATA_HOME:-$HOME/.local/share}/fonts/$name

    if ! forced && [ -f "$fontdir/.installed" ]; then
        printe_info "$fontdir already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive \
        "$name.tar.gz" \
        "$repo" \
        "$ver"

    verify_shasum "$name.tar.gz" "$shasum"
    run_tar -C "$BUILD_DIR" -xzf "$name.tar.gz"
    _install_font "$BUILD_DIR/$name-$(echo "$ver" | tr "/" "-")" "$fontdir"
}

_install_font_url() {
    name=$1; shift
    url=$1; shift
    shasum=$1; shift

    fontdir=${XDG_DATA_HOME:-$HOME/.local/share}/fonts/$name
    basename=$(basename "$url")
    unzip=

    if ! forced && [ -f "$fontdir/.installed" ]; then
        printe_info "$fontdir already exists, skipping..."
        return
    fi

    case "$basename" in
        *.zip )
            if [ -z "$unzip" ] && command -v unzip >/dev/null; then
                unzip=unzip
            fi

            if [ -z "$unzip" ] && command -v bsdtar >/dev/null; then
                unzip=bsdtar
            fi

            if [ -z "$unzip" ]; then
                printe_info "unzip binary does not exists, skipping..."
                return 1
            fi
            ;;
    esac

    cd "$BUILD_DIR" || exit 1

    fetch_url "$basename" "$url"
    verify_shasum "$basename" "$shasum"

    case "$basename" in
        *.tar.gz )
            run_tar -C "$BUILD_DIR" -xzf "$basename"
            ;;

        *.zip )
            mkdir -p "$BUILD_DIR/$name"
            case "$unzip" in
                bsdtar )
                    bsdtar -C "$BUILD_DIR/$name" -xvf "$basename"
                    ;;

                unzip )
                    unzip -d "$BUILD_DIR/$name" "$basename"
                    ;;
            esac
            ;;

        *.ttf | *.ttc )
            mkdir -p "$BUILD_DIR/$name"
            install -m0644 "$basename" "$BUILD_DIR/$name/"
    esac

    _install_font "$BUILD_DIR/$name" "$fontdir"
}

_run() {
    printe_h2 "Installing fonts..."

    ## Adobe Source
    ##

    _install_font_gh \
        source-code-pro \
        adobe/source-code-pro \
        a4e4dd59b8e0a436b934f0f612c2e91b5932910c6d1c3b7d1a5a9f389c86302b \
        2.030R-ro/1.050R-it

    _install_font_gh \
        source-sans-pro \
        adobe/source-sans-pro \
        01e78d7ff451545ff1eec6cf14b28f62135e430a7ba80d74a90efd5334fef7eb \
        2.045R-ro/1.095R-it

    _install_font_gh \
        source-serif-pro \
        adobe/source-serif-pro \
        bbb504463342f01666db34790574477b2bbd61a338897466461c66c4cd4464fd \
        3.000R

    ## Adobe Source Han
    ## Full archive for Source Han family are around 1.5GB, so instead we're
    ## downloading the font directly.
    ##

    adobe_gh=https://github.com/adobe-fonts

    _install_font_url \
        source-han-sans \
        $adobe_gh/source-han-sans/releases/download/2.001R/SourceHanSans.ttc \
        9e94fe493685a7c99ce61e4488169007e3b97badb9f1ef43d3c13da501463780

    _install_font_url \
        source-han-serif \
        $adobe_gh/source-han-serif/releases/download/1.001R/SourceHanSerif.ttc \
        85cc634fa228286ca307803bbb4ca61f61fa821b3ed573f4f07c6f0c064426b5

    ## Hack Fonts
    ##

    hack_gh=https://github.com/source-foundry/Hack

    _install_font_url \
        hack \
        $hack_gh/releases/download/v3.003/Hack-v3.003-ttf.tar.gz \
        7f592d091cbd2472f39af96e6c280cde3c27cab50a9c88746feb42918aa1888f

    ## Roboto
    ##

    roboto_gh=https://github.com/google/roboto

    _install_font_url \
        roboto \
        $roboto_gh/releases/download/v2.138/roboto-android.zip \
        c825453253f590cfe62557733e7173f9a421fff103b00f57d33c4ad28ae53baf

    ## Droid Sans
    ## Using local mirror, since Droid Sans has been removed from Google Fonts
    ## in favor for Noto, but Noto Thai wasn't really designed for reading
    ##

    _install_font_url \
        droid-sans-thai \
        https://files.grid.in.th/pub/fonts/droid-sans-thai.tar.gz \
        c0f2ab8b3471c3b5ecca4c94400ad489a7e09b811739bdea537c7f4ef9be6a80

    _install_font_url \
        droid-serif-thai \
        https://files.grid.in.th/pub/fonts/droid-serif-thai.tar.gz \
        8d27167379ea9718530d35c5872e65f5a09adc99fe505c4d506e9bbaaefe84d7
}

run_with_flavors "$@"
