function virtualenv -d "Manage Python virtualenv environment"

    # Set virtualenv base directory
    if not set -q __virtualenvs
        set -g __virtualenvs ~/.virtualenvs
    end

    # Check whether Virtualenv exists before running.
    if test -f /usr/local/share/python/virtualenv

        switch "$argv[1]"

        # Deactivate old Virtualenv and append Virtualenv to $PATH.
        case activate

            virtualenv deactivate
            if test -d $__virtualenvs/$argv[2]
                set -g __virtualenv_path $PATH
                set -g __virtualenv_home $PYTHON_HOME
                set -g PATH $__virtualenvs/$argv[2]/bin $__virtualenv_path
                set -g PYTHON_HOME $__virtualenvs/$argv[2]
            else
                echo "Unable to activate Virtualenv: $argv[2] not found."
                echo "Maybe you want to create a new Virtualenv?"
                echo
                echo "    virtualenv new $argv[2]"
                echo
            end

        # Remove Virtualenv PATH from PATH env.
        case deactivate
            if set -q __virtualenv_path
                set -g PATH $__virtualenv_path
                set -e __virtualenv_path
            end

            if set -q __virtualenv_home
                set -g PYTHON_HOME $__virtualenv_home
                set -e __virtualenv_home
            end

        # Create new Virtualenv in virtualenvs directory.
        case new
            set __virtualenv_pwd (pwd)
            cd $__virtualenvs
            /usr/local/share/python/virtualenv $argv[(seq 2 (count $argv))]
            virtualenv activate $argv[(count $argv)]
            cd $__virtualenv_pwd

        case '*'
            echo "Usage: virtualenv SUBCOMMAND ARGS"
            echo "Where SUBCOMMAND is one of the following:"
            echo
            echo "    activate NAME        Activate the Virtualenv"
            echo "    deactivate           Deactivate the Virtualenv"
            echo "    new [OPTIONS] NAME   Create a new Virtualenv"
            echo
        end

    else
        echo "Could not find Virtualenv."
        echo "Are you sure it is installed?"
    end

end
