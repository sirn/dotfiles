function emacs
    if test -d /Applications/Emacs.app/
        command /Applications/Emacs.app/Contents/MacOS/Emacs -nw
    else
        command emacs -nw $argv
    end
end
