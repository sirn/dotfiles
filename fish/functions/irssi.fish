function irssi
    if which tmux 2>&1 >/dev/null
        command tmux new -A -s irssi command irssi $argv \; set status off
    else
        command irssi $argv
    end
end
