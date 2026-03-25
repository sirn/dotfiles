{
  config,
  lib,
  pkgs,
  ...
}:

let
  claudeCfg = config.programs.claude-code;
  piCfg = config.programs.pi-coding-agent;
  opencodeCfg = config.programs.opencode;

  rtkBin = lib.getExe pkgs.unstable.rtk;
  tomlFormat = pkgs.formats.toml { };

  # Commands to ignore (not proxy commands)
  ignoreCommands = [
    "cc-economics"
    "config"
    "deps"
    "diff"
    "discover"
    "env"
    "err"
    "gain"
    "help"
    "hook"
    "hook-audit"
    "init"
    "json"
    "learn"
    "log"
    "proxy"
    "read"
    "rewrite"
    "smart"
    "summary"
    "test"
    "verify"
  ]
  ++ rtkConfig.hooks.exclude_commands;

  # Generate RTK instructions dynamically from `rtk --help` output
  rtkInstructionTextFile =
    pkgs.runCommand "rtk-instructions.md"
      {
        nativeBuildInputs = [ pkgs.unstable.rtk ];
        ignore = lib.concatStringsSep " " ignoreCommands;
      }
      ''
        cat > $out << 'HEADER'
        ## RTK (Shell Output Optimization)

        RTK (`rtk`) is available for compact shell output. Use `rtk --help` to see all available commands.

        ### Rewrite mode

        The following commands are automatically rewritten by RTK (e.g. calling `ls` with bash tool will be automatically rewritten to use `rtk ls`) and should NOT be additionally filtered/parsed with `tail`, `head`, `jq`, etc. You do not typically need to call `rtk ...` on your own. If you MUST call the original command, use `command ...` (e.g. `command ls`):

        HEADER

        ${rtkBin} --help 2>/dev/null | ${pkgs.gawk}/bin/awk -v ignore="$ignore" '
        BEGIN {
          split(ignore, ignarr, " ");
          for (i in ignarr) is_ignored[ignarr[i]] = 1;
        }

        /^Commands:/ { in_commands = 1; next }
        /^Options:/ { in_commands = 0 }

        in_commands && /^  [a-z0-9-]+/ {
          cmd = $1;

          # Skip ignored commands
          if (is_ignored[cmd]) next;

          # Extract description
          desc = substr($0, index($0, $2));
          gsub(/^[ \t]+/, "", desc);
          gsub(/[ \t]+/, " ", desc);

          printf "- `rtk %s <args>` — %s (replaces `%s`)\n", cmd, desc, cmd;
        }
        ' >> $out
      '';

  rtkInstructionText = builtins.readFile rtkInstructionTextFile;

  rtkRewriteClaudeSh = pkgs.writeShellApplication {
    name = "rtk-rewrite-claude";
    runtimeInputs = [
      pkgs.jq
      pkgs.unstable.rtk
    ];
    text = builtins.readFile ./rtk-rewrite-claude.sh;
  };

  rtkRewritePiTs = builtins.replaceStrings [ "__RTK_BIN__" ] [ rtkBin ] (
    builtins.readFile ./rtk-rewrite-pi.ts
  );

  rtkRewriteOpencodeTs = builtins.replaceStrings [ "__RTK_BIN__" ] [ rtkBin ] (
    builtins.readFile ./rtk-rewrite-opencode.ts
  );

  rtkConfig = {
    hooks = {
      exclude_commands = [ "curl" ];
    };
  };
in
{
  home.packages = [ pkgs.unstable.rtk ];

  home.file = {
    "Library/Application Support/rtk/config.toml" = lib.mkIf pkgs.stdenv.isDarwin {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    ".pi/agent/extensions/rtk-rewrite.ts" = lib.mkIf piCfg.enable { text = rtkRewritePiTs; };
  };

  xdg.configFile = {
    "rtk/config.toml" = lib.mkIf pkgs.stdenv.isLinux {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    "opencode/plugins/rtk-rewrite.ts" = lib.mkIf opencodeCfg.enable { text = rtkRewriteOpencodeTs; };
  };

  programs.claude-code.settings = lib.mkIf claudeCfg.enable {
    hooks = {
      PreToolUse = [
        {
          matcher = "Bash";
          hooks = [
            {
              type = "command";
              command = "${rtkRewriteClaudeSh}/bin/rtk-rewrite-claude";
            }
          ];
        }
      ];
    };
  };

  agents.instructionText = lib.mkAfter rtkInstructionText;

  # OpenCode evaluates permissions on the rewritten command (after tool.execute.before hook),
  # not the original. These entries allow RTK-rewritten commands to pass permission checks.
  # Deny/ask rules use substring mode and already match RTK-prefixed commands
  # (e.g., "* git push *" matches "rtk git push origin main"), so only allow entries are needed.
  #
  # TODO: Remove once permission.ask plugin hook is implemented upstream:
  # https://github.com/anomalyco/opencode/issues/7006
  agents.permissions.default.commands.allow = lib.mkIf opencodeCfg.enable (
    lib.mkAfter [
      # cat/head/tail → rtk read
      "rtk read"
      # cargo → rtk cargo
      "rtk cargo"
      "rtk cargo check"
      "rtk cargo clippy"
      "rtk cargo fmt --check"
      "rtk cargo test"
      "rtk cargo tree"
      # curl → rtk curl
      "rtk curl"
      # docker → rtk docker
      "rtk docker images"
      "rtk docker inspect"
      "rtk docker ps"
      "rtk docker ps -a"
      # eslint → rtk lint
      "rtk lint"
      # find → rtk find
      "rtk find"
      # gh → rtk gh
      "rtk gh api --method GET"
      "rtk gh api -X GET"
      "rtk gh cache list"
      "rtk gh issue list"
      "rtk gh issue status"
      "rtk gh issue view"
      "rtk gh pr checks"
      "rtk gh pr diff"
      "rtk gh pr list"
      "rtk gh pr status"
      "rtk gh pr view"
      "rtk gh release list"
      "rtk gh release view"
      "rtk gh repo list"
      "rtk gh repo view"
      "rtk gh run list"
      "rtk gh run view"
      "rtk gh search code"
      "rtk gh search commits"
      "rtk gh search issues"
      "rtk gh search prs"
      "rtk gh search repos"
      "rtk gh status"
      "rtk gh workflow list"
      "rtk gh workflow view"
      # git → rtk git (only the specific allowed subcommands, not config/remote/rev-parse)
      "rtk git branch"
      "rtk git diff"
      "rtk git log"
      "rtk git status"
      # go → rtk go (only build/test/vet, not fmt/mod)
      "rtk go build"
      "rtk go test"
      "rtk go vet"
      # golangci-lint → rtk golangci-lint
      "rtk golangci-lint"
      # grep/rg → rtk grep
      "rtk grep"
      # kubectl → rtk kubectl
      "rtk kubectl describe"
      "rtk kubectl get"
      "rtk kubectl logs"
      # ls → rtk ls
      "rtk ls"
      # mypy → rtk mypy
      "rtk mypy"
      # pip → rtk pip
      "rtk pip freeze"
      "rtk pip list"
      # pnpm → rtk pnpm
      "rtk pnpm list"
      # prettier → rtk prettier
      "rtk prettier --check"
      # psql → rtk psql
      "rtk psql --command=\\dt"
      "rtk psql -c \\dt"
      # pytest/python -m pytest → rtk pytest
      "rtk pytest"
      # ruff → rtk ruff
      "rtk ruff"
      "rtk ruff check"
      "rtk ruff format --check"
      # tree → rtk tree
      "rtk tree"
      # tsc → rtk tsc
      "rtk tsc --noEmit"
      "rtk tsc -p . --noEmit"
      # wget → rtk wget
      "rtk wget"
    ]
  );
}
