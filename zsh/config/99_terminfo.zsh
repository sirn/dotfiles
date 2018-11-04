case "$TERM" in
    screen*) ;;
    *)
        TERM="xterm-256color"
        if infocmp xterm-24bits >/dev/null 2>&1; then
            TERM="xterm-24bits"
        fi
        ;;
esac
