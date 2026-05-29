{ config, ... }:
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
        "awk --version"
        "bash -n"
        "bat"
        "batcat"
        "black --check"
        "bundle exec rspec"
        "bundle exec rubocop"
        "bundle list"
        "cargo"
        "cat"
        "clang-tidy"
        "command -v"
        "cppcheck"
        "curl"
        "cut"
        "date"
        "diff"
        "docker images"
        "docker inspect"
        "docker logs"
        "docker ps"
        "docker-compose config"
        "echo"
        "eslint"
        "fd"
        "file"
        "find"
        "flake8"
        "gem list"
        "gh api"
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
        "helm env"
        "helm get all"
        "helm get hooks"
        "helm get manifest"
        "helm get notes"
        "helm get values"
        "helm history"
        "helm ls"
        "helm repo list"
        "helm search hub"
        "helm search repo"
        "helm show all"
        "helm show chart"
        "helm show crds"
        "helm show readme"
        "helm show values"
        "helm status"
        "helm template"
        "helm version"
        "id"
        "isort --check-only"
        "jj bookmark list"
        "jj commit"
        "jj describe"
        "jj diff"
        "jj evolog"
        "jj file annotate"
        "jj file list"
        "jj file search"
        "jj file show"
        "jj git remote list"
        "jj log"
        "jj new"
        "jj split"
        "jj show"
        "jj status"
        "jaq"
        "jq"
        "just"
        "ktlint"
        "kubectl describe"
        "kubectl get"
        "kubectl logs"
        "kubectl version"
        "ls"
        "lstr"
        "make"
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
        "nix-instantiate"
        "node --version"
        "npm --version"
        "npm list"
        "pg_dump --schema-only"
        "pip freeze"
        "pip list"
        "pnpm list"
        "podman images"
        "podman inspect"
        "podman logs"
        "podman ps"
        "poetry run pytest"
        "poetry run python -m pytest"
        "poetry run ruff"
        "poetry show"
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
        "ruff format --check"
        "rustc --version"
        "sed --version"
        "sed -n"
        "shellcheck"
        "shfmt"
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
        "uniq"
        "uv pip list"
        "uv pip tree"
        "uv run pytest"
        "uv run python -m pytest"
        "uv run ruff"
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
        "jj bookmark delete"
        "jj bookmark move"
        "jj bookmark set"
        "jj edit"
        "jj op restore"
        "jj rebase"
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
        # HTTP method flags (-X / --request / --method) — any command
        {
          match = "*:-X POST";
          mode = "args";
        }
        {
          match = "*:-XPOST";
          mode = "args";
        }
        {
          match = "*:--request POST";
          mode = "args";
        }
        {
          match = "*:--request=POST";
          mode = "args";
        }
        {
          match = "*:-X PUT";
          mode = "args";
        }
        {
          match = "*:-XPUT";
          mode = "args";
        }
        {
          match = "*:--request PUT";
          mode = "args";
        }
        {
          match = "*:--request=PUT";
          mode = "args";
        }
        {
          match = "*:-X DELETE";
          mode = "args";
        }
        {
          match = "*:-XDELETE";
          mode = "args";
        }
        {
          match = "*:--request DELETE";
          mode = "args";
        }
        {
          match = "*:--request=DELETE";
          mode = "args";
        }
        {
          match = "*:-X PATCH";
          mode = "args";
        }
        {
          match = "*:-XPATCH";
          mode = "args";
        }
        {
          match = "*:--request PATCH";
          mode = "args";
        }
        {
          match = "*:--request=PATCH";
          mode = "args";
        }
        {
          match = "*:--method DELETE";
          mode = "args";
        }
        {
          match = "*:--method=DELETE";
          mode = "args";
        }
        {
          match = "*:--method PATCH";
          mode = "args";
        }
        {
          match = "*:--method=PATCH";
          mode = "args";
        }
        {
          match = "*:--method POST";
          mode = "args";
        }
        {
          match = "*:--method=POST";
          mode = "args";
        }
        {
          match = "*:--method PUT";
          mode = "args";
        }
        {
          match = "*:--method=PUT";
          mode = "args";
        }
        # curl: data-sending flags (implicitly POST)
        {
          match = "curl:-d";
          mode = "args";
        }
        {
          match = "curl:--data";
          mode = "args";
        }
        {
          match = "curl:--data-binary";
          mode = "args";
        }
        {
          match = "curl:--data-raw";
          mode = "args";
        }
        {
          match = "curl:--data-urlencode";
          mode = "args";
        }
        {
          match = "curl:-F";
          mode = "args";
        }
        {
          match = "curl:--form";
          mode = "args";
        }
        {
          match = "curl:--form-string";
          mode = "args";
        }
        # curl: upload flag (implicitly PUT)
        {
          match = "curl:-T";
          mode = "args";
        }
        {
          match = "curl:--upload-file";
          mode = "args";
        }
        # gh api: mutation flags
        {
          match = "gh api:--input";
          mode = "args";
        }
        {
          match = "gh api:-f";
          mode = "args";
        }
        {
          match = "gh api:--field";
          mode = "args";
        }
        {
          match = "gh api:-F";
          mode = "args";
        }
        {
          match = "gh api:--raw-field";
          mode = "args";
        }
      ];

      commands.deny = [
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
        "git push"
        "jj git push"
        {
          match = "jj:--ignore-immutable";
          mode = "args";
        }
        {
          match = "jj:--ignore-working-copy";
          mode = "args";
        }
        {
          match = "jj:--at-operation";
          mode = "args";
        }
        "kill"
        "sops"
        "doas"
        "sudo"
        "find /"
        "find /nix"
        "find /nix/store"
        "find ${config.home.homeDirectory}"
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

      delegate = {
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

      subagent = { };

      "subagent:researcher" = {
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

      "subagent:worker" = {
        tools = {
          edit = true;
          write = true;
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
