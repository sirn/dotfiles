if status --is-interactive
    set -x SHELL $__fish_bin_dir/fish
    set fish_greeting ""

    if type -P direnv >/dev/null 2>&1
        direnv hook fish | source
    end
end
