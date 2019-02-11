#!/usr/local/bin/env zsh
#
# This preexec hook make sure gpg-agent startuptty is set to the current
# tty before running anything that may invoke ssh-agent.
#
# This is only required when gpg-agent is being used as ssh-agent, as
# ssh-agent protocol has no way to pass the correct TTY to gpg-agent.
# See also --enable-ssh-support in gpg-agent(1)
#

case "$TERM" in
    xterm* | rxvt* | screen* )
        _gpg_update_tty_hook () {
            case "$1" in
                *ssh* | *sftp* | *scp* | *git* | *rsync* | *mosh* | *ghq* | *duplicity* )
                    gpg-connect-agent updatestartuptty /bye >/dev/null
                    ;;
                * )
                    ;;
            esac
        }

        add-zsh-hook preexec _gpg_update_tty_hook
        ;;

    eterm* )
        ;;

    * )
        ;;
esac
