if has_homebrew

    # Set PYTHON_PATH, use whatever comes first
    brew --prefix python python3 pypy| while read prefix
        if not set -q PYTHON_PATH
            set PYTHON_PATH $prefix/bin
            break
        end
    end

end

