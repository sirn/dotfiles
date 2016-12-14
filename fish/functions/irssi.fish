function irssi
    set irssi_bin (type -P irssi)
    if type -P tmux >/dev/null 2>&1
      tmux new -A -s irssi $irssi_bin $argv \; set status off
    else
      eval $irssi_bin $argv
    end
end
