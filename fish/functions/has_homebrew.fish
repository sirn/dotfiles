function has_homebrew --description 'Check if Homebrew exists'

    # Check if file exists and cache the result
    if not set -q __homebrew_available
        if type -p brew > /dev/null
            set -g __homebrew_available true
        else
            set -g __homebrew_available false
        end
    end

    # Eval true or false to return status code
    eval $__homebrew_available

end
