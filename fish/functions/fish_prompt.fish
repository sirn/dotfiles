function fish_prompt --description 'Write out the prompt'

    # Calculate these once, to save a few cycles when displaying the prompt
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    # Color for host section.
    if not set -q __fish_prompt_host
        set -g __fish_prompt_host (set_color -b 444444 ffffff)
    end

    # Reset color.
    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color normal)
    end

    # Main switch, display prompt using different color for root.
    switch $USER
    case root

        if not set -q __fish_prompt_cwd
            if set -q fish_color_cwd_root
                set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
            else
                set -g __fish_prompt_cwd (set_color $fish_color_cwd)
            end
        end

    case '*'

        if not set -q __fish_prompt_cwd
            set -g __fish_prompt_cwd (set_color -b $fish_color_cwd ffffff)
        end

    end

    echo -n -s \
        "$__fish_prompt_host" " $__fish_prompt_hostname " \
        "$__fish_prompt_normal" "$__fish_prompt_cwd " (prompt_pwd) " " \
        "$__fish_prompt_normal" " "
end
