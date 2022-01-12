#!/bin/sh -e
#
# Install fonts.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"

_install_font() {
    srcdir=$1; shift
    dest=$1; shift

    rm -rf "$dest" || exit 1
    install -d "$dest" || exit 1

    find "$srcdir" \
         \( -iname "*.ttf" -or -iname "*.ttc" -or -name "*.otf" \) \
         -exec install -m0644 \{\} "$dest" \; || exit 1

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
            mkdir -p "$BUILD_DIR/$name"
            run_tar -C "$BUILD_DIR/$name" -xzf "$basename"
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
    ## Adobe Source
    ##

    _install_font_gh \
        source-code-pro \
        adobe/source-code-pro \
        d8f879fefd66ecb7291ea2d73984b9c0a97fb499424e58290ba6b81787d0c725 \
        2.038R-ro/1.058R-it/1.018R-VAR

    _install_font_gh \
        source-sans \
        adobe/source-sans-pro \
        23df371ce8ce0c5c57e750f1b01af6a31e437fe1737df0501b8181162642686a \
        3.028R

    _install_font_gh \
        source-serif \
        adobe/source-serif-pro \
        dfb364735699cb830caad534cf7741234804d28e4b6fc5e4736b2401f6131aba \
        4.004R

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

    ## Noto Fonts
    ##

    google_url=https://noto-website-2.storage.googleapis.com/pkgs

    _install_font_url \
        noto-sans \
        $google_url/NotoSans-hinted.zip \
        02e5f834c526f815f3b9f90f98e2b5b09f17cec107cc751c3b58fc3a2c0b2942

    _install_font_url \
        noto-serif \
        $google_url/NotoSerif-hinted.zip \
        64e4c71c5b0d09f41c4093c1f673d92350eb8789f041d1f5e948ccfbdbe0bafa

    ## Hack Fonts
    ##

    hack_gh=https://github.com/source-foundry/Hack

    _install_font_url \
        hack \
        $hack_gh/releases/download/v3.003/Hack-v3.003-ttf.tar.gz \
        7f592d091cbd2472f39af96e6c280cde3c27cab50a9c88746feb42918aa1888f

    ## Twemoji
    ##

    twemoji_gh=https://github.com/eosrei/twemoji-color-font
    _install_font_url \
        twemoji \
        $twemoji_gh/releases/download/v12.0.1/TwitterColorEmoji-SVGinOT-Linux-12.0.1.tar.gz \
        7886d78ecc992f87e17c9cf111d1e41100b0c5a5203c44c595faccb0ce14becd

    ## iA Writer
    ##

    iawriter_gh=https://github.com/iaolo/iA-Fonts
    _install_font_url \
        ia-writer-fonts \
        $iawriter_gh/archive/ba31f88a1e71f413f97bbd23a99e21b29d4c3a28.zip \
        e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855

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

    ## IBM Plex
    ##

    _install_font_url \
        ibm-plex \
        https://github.com/IBM/plex/releases/download/v4.0.2/OpenType.zip \
        e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
}

run_with_flavors "$@"
