function ytwatch
    command youtube-dl -o - $argv | mpv -
end
