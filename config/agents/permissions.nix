{
  agents.permissions = {
    default = {
      tools = {
        read = true;
        glob = true;
        grep = true;
        list = true;
        webfetch = true;
        websearch = true;
      };

      commands.allow = [
        "./gradlew check"
        "["
        "alejandra"
        "alejandra --check"
        "awk --version"
        "bash -n"
        "bat"
        "batcat"
        "black --check"
        "bundle exec rspec"
        "bundle exec rubocop"
        "bundle list"
        "cargo"
        "cargo --version"
        "cargo check"
        "cargo clippy"
        "cargo fmt --check"
        "cargo test"
        "cargo tree"
        "cat"
        "cat --version"
        "clang-tidy"
        "command -v"
        "cppcheck"
        "curl"
        "cut"
        "date"
        "docker images"
        "docker inspect"
        "docker ps"
        "docker ps -a"
        "docker-compose config"
        "echo"
        "eslint"
        "fd"
        "file"
        "find"
        "flake8"
        "gem list"
        "gh api --method GET"
        "gh api -X GET"
        "gh cache list"
        "gh issue list"
        "gh issue status"
        "gh issue view"
        "gh pr checks"
        "gh pr diff"
        "gh pr list"
        "gh pr status"
        "gh pr view"
        "gh release list"
        "gh release view"
        "gh repo list"
        "gh repo view"
        "gh run list"
        "gh run view"
        "gh search code"
        "gh search commits"
        "gh search issues"
        "gh search prs"
        "gh search repos"
        "gh status"
        "gh workflow list"
        "gh workflow view"
        "git branch"
        "git config --get"
        "git config --list"
        "git diff"
        "git log"
        "git remote -v"
        "git remote list"
        "git rev-parse"
        "git rev-parse --show-toplevel"
        "git status"
        "gmake"
        "go build"
        "go fmt"
        "go mod download -x"
        "go mod graph"
        "go mod why"
        "go test"
        "go vet"
        "golangci-lint"
        "gradle check"
        "grep"
        "head"
        "id"
        "isort --check-only"
        "jj bookmark"
        "jj bookmark list"
        "jj diff"
        "jj git remote list"
        "jj log"
        "jj show"
        "jj status"
        "jq"
        "just"
        "just --list"
        "ktlint"
        "kubectl describe"
        "kubectl get"
        "kubectl logs"
        "kubectl version"
        "ls"
        "lstr"
        "make"
        "make check"
        "mvn test"
        "mvn verify"
        "mypy"
        "mysqlshow"
        "nix build"
        "nix derivation show"
        "nix eval"
        "nix flake"
        "nix path-info"
        "nix search"
        "nix why-depends"
        "nix-build"
        "nix-channel --list"
        "nix-linter"
        "nix-prefetch-git"
        "nixfmt"
        "nixfmt --check"
        "node --version"
        "npm --version"
        "npm list"
        "pg_dump --schema-only"
        "pip freeze"
        "pip list"
        "pnpm list"
        "podman images"
        "podman inspect"
        "podman ps"
        "podman ps -a"
        "poetry run pytest"
        "poetry run python -m pytest"
        "poetry run ruff"
        "poetry run ruff check"
        "poetry show"
        "poetry show --tree"
        "prettier --check"
        "psql --command=\\dt"
        "psql -c \\dt"
        "cd"
        "pwd"
        "pylint"
        "pytest"
        "python -m pytest"
        "python -m unittest"
        "redis-cli info"
        "redis-cli ping"
        "rg"
        "rtk deps"
        "rtk gain"
        "rtk help"
        "rtk json"
        "rtk log"
        "rubocop"
        "ruff"
        "ruff check"
        "ruff format --check"
        "rustc --version"
        "sed --version"
        "sed -n"
        "shellcheck"
        "shfmt"
        "shfmt --check"
        "shfmt -l"
        "sleep"
        "sort"
        "stat"
        "staticcheck"
        "tail"
        "test"
        "tmux capture-pane"
        "tr"
        "tree"
        "tsc --noEmit"
        "tsc -p . --noEmit"
        "uname"
        "uname -a"
        "uniq"
        "uv pip list"
        "uv pip tree"
        "uv run pytest"
        "uv run python -m pytest"
        "uv run ruff"
        "uv run ruff check"
        "wc"
        "wget"
        "which"
        "whoami"
        "yarn list"
      ];

      commands.ask = [
        "chmod"
        "chown"
        "dd"
        {
          match = "env";
          mode = "exact";
        }
        "fdisk"
        "git commit"
        "jj abandon"
        "jj commit"
        "jj describe"
        "jj new"
        "jj split"
        "jj squash"
        "jj undo"
        "kubectl apply"
        "kubectl create"
        "kubectl delete"
        "kubectl exec"
        "kubectl run"
        "mkfs"
        "nix run"
        {
          match = "printenv";
          mode = "exact";
        }
        "rm"
        "rtk env"
        "shred"
      ];

      commands.deny = [
        {
          match = "--method DELETE";
          mode = "substring";
        }
        {
          match = "--method=DELETE";
          mode = "substring";
        }
        {
          match = "--method PATCH";
          mode = "substring";
        }
        {
          match = "--method=PATCH";
          mode = "substring";
        }
        {
          match = "--method POST";
          mode = "substring";
        }
        {
          match = "--method=POST";
          mode = "substring";
        }
        {
          match = "--method PUT";
          mode = "substring";
        }
        {
          match = "--method=PUT";
          mode = "substring";
        }
        "gh api --input"
        "gh api -f"
        "gh api --field"
        "gh api -F"
        "gh api --raw-field"
        "gh issue create"
        "gh issue close"
        "gh issue delete"
        "gh issue edit"
        "gh issue reopen"
        "gh pr create"
        "gh pr close"
        "gh pr edit"
        "gh pr merge"
        "gh pr reopen"
        "gh release create"
        "gh release delete"
        "gh repo create"
        "gh repo delete"
        "gh repo fork"
        {
          match = "-X POST";
          mode = "substring";
        }
        {
          match = "-XPOST";
          mode = "substring";
        }
        {
          match = "--request POST";
          mode = "substring";
        }
        {
          match = "--request=POST";
          mode = "substring";
        }
        {
          match = "-X PUT";
          mode = "substring";
        }
        {
          match = "-XPUT";
          mode = "substring";
        }
        {
          match = "--request PUT";
          mode = "substring";
        }
        {
          match = "--request=PUT";
          mode = "substring";
        }
        {
          match = "-X DELETE";
          mode = "substring";
        }
        {
          match = "-XDELETE";
          mode = "substring";
        }
        {
          match = "--request DELETE";
          mode = "substring";
        }
        {
          match = "--request=DELETE";
          mode = "substring";
        }
        {
          match = "-X PATCH";
          mode = "substring";
        }
        {
          match = "-XPATCH";
          mode = "substring";
        }
        {
          match = "--request PATCH";
          mode = "substring";
        }
        {
          match = "--request=PATCH";
          mode = "substring";
        }
        "git push"
        "jj git push"
        "kill"
        "sops"
        "doas"
        "sudo"
        "systemctl"
        "find /nix/store"
      ];

      wrappers = [
        {
          name = "bash";
          kind = "shell-c";
        }
        {
          name = "sh";
          kind = "shell-c";
        }
        {
          name = "zsh";
          kind = "shell-c";
        }
        {
          name = "dash";
          kind = "shell-c";
        }
        {
          name = "ksh";
          kind = "shell-c";
        }
        {
          name = "sudo";
          kind = "utility-operand";
        }
        {
          name = "doas";
          kind = "utility-operand";
        }
        {
          name = "time";
          kind = "utility-operand";
        }
        {
          name = "nohup";
          kind = "utility-operand";
        }
        {
          name = "nice";
          kind = "utility-operand";
        }
        {
          name = "chroot";
          kind = "utility-operand";
        }
        {
          name = "timeout";
          kind = "utility-operand";
        }
        {
          name = "setsid";
          kind = "utility-operand";
        }
        {
          name = "command";
          kind = "utility-operand";
        }
        {
          name = "env";
          kind = "env";
        }
        {
          name = "xargs";
          kind = "xargs";
        }
        {
          name = "docker";
          kind = "docker-run";
        }
        {
          name = "podman";
          kind = "docker-run";
        }
      ];

      redirects = {
        action = "allow";
      };
      heredocs = {
        action = "ask";
      };

      paths = {
        allow = {
          read = [
            "**/*.env.example"
            "**/*.env.sample"
            "**/.env.example"
            "**/.env.sample"
          ];
          edit = [
            "**/*.env.example"
            "**/*.env.sample"
            "**/.env.example"
            "**/.env.sample"
          ];
          write = [
            "**/*.env.example"
            "**/*.env.sample"
            "**/.env.example"
            "**/.env.sample"
          ];
        };
        deny = {
          read = [
            "**/.env"
            "**/.env.*"
            "**/*.env"
            "**/*.pem"
            "**/*.key"
            "**/*.p12"
            "**/*.pfx"
            "**/*.jks"
            "**/*.keystore"
            "**/id_rsa"
            "**/id_ed25519"
            "**/*credential*.json"
            "**/*credentials*.json"
            "**/*secret*"
            "**/*secrets*"
            "**/*token*"
            "**/*apikey*"
            "**/*api_key*"
            "**/.aws/credentials"
            "**/.npmrc"
            "**/.pypirc"
            "**/.netrc"
            "**/.docker/config.json"
            "**/sops*.yml"
            "**/sops*.yaml"
            "**/*.agekey"
          ];
          edit = [
            "**/.env"
            "**/.env.*"
            "**/*.env"
            "**/*.pem"
            "**/*.key"
            "**/*.p12"
            "**/*.pfx"
            "**/*.jks"
            "**/*.keystore"
            "**/id_rsa"
            "**/id_ed25519"
            "**/*credential*.json"
            "**/*credentials*.json"
            "**/*secret*"
            "**/*secrets*"
            "**/*token*"
            "**/*apikey*"
            "**/*api_key*"
            "**/.aws/credentials"
            "**/.npmrc"
            "**/.pypirc"
            "**/.netrc"
            "**/.docker/config.json"
            "**/sops*.yml"
            "**/sops*.yaml"
            "**/*.agekey"
          ];
          write = [
            "**/.env"
            "**/.env.*"
            "**/*.env"
            "**/*.pem"
            "**/*.key"
            "**/*.p12"
            "**/*.pfx"
            "**/*.jks"
            "**/*.keystore"
            "**/id_rsa"
            "**/id_ed25519"
            "**/*credential*.json"
            "**/*credentials*.json"
            "**/*secret*"
            "**/*secrets*"
            "**/*token*"
            "**/*apikey*"
            "**/*api_key*"
            "**/.aws/credentials"
            "**/.npmrc"
            "**/.pypirc"
            "**/.netrc"
            "**/.docker/config.json"
            "**/sops*.yml"
            "**/sops*.yaml"
            "**/*.agekey"
          ];
        };
      };
    };

    modes = {
      plan = {
        tools = {
          edit = false;
          write = false;
        };
        commands.deny = [
          "cp"
          "ln"
          "mkdir"
          "mktemp"
          "mv"
          "rsync"
          "sed -i"
          "tee"
          "touch"
        ];
        redirects = {
          action = "deny";
          safeTargets = [
            "/dev/null"
            "/dev/stderr"
            "/dev/stdout"
          ];
          allowFdDup = true;
        };
        heredocs = {
          action = "ask";
        };
      };

      build = {
        tools = {
          edit = true;
          write = true;
        };
      };
    };
  };
}
