if status --is-interactive
    set -x SHELL $__fish_bin_dir/fish

    if type -P direnv >/dev/null 2>&1
       eval (direnv hook fish)
    end
end
