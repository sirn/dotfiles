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

  rtkSkillSet =
    pkgs.runCommand "rtk-skill-set"
      {
        nativeBuildInputs = [ pkgs.unstable.rtk ];
        ignore = lib.concatStringsSep " " ignoreCommands;
      }
      ''
        mkdir -p $out/rtk

        cat > $out/rtk/SKILL.md <<'HEADER'
        ---
        name: rtk
        type: reference
        description: Reference for RTK shell output optimization and command rewrite behavior. ALWAYS read when using, configuring, or reasoning about RTK-rewritten shell commands.
        ---

        ## Overview

        RTK (`rtk`) optimizes shell output for agent sessions. It rewrites common commands to compact RTK subcommands and filters noisy output from tools such as test runners, package managers, cloud CLIs, and formatters.

        ## Agent Behavior

        - Shell commands may be automatically rewritten through `rtk rewrite` before execution.
        - Prefer the normal command (`ls`, `pytest`, `cargo test`, etc.) and let the installed hook rewrite it.
        - Do not add extra `head`, `tail`, `jq`, or similar filters just to reduce output when RTK already handles the command.
        - If you must bypass rewrite behavior, prefix the command with `command`, for example: `command ls`.
        - Use `rtk --help` to inspect the currently installed command list.
        - Use `rtk rewrite '<command>'` to preview how a command will be transformed.

        ## Direct RTK Commands

        These are RTK-native utilities rather than transparent command rewrites:

        - `rtk --help` - show available commands
        - `rtk rewrite '<command>'` - preview rewrite behavior
        - `rtk read <file>` - compact file reads
        - `rtk json ...` - compact JSON processing helpers
        - `rtk log ...` - inspect RTK logs
        - `rtk env` - inspect RTK environment/debug state
        - `rtk deps` - inspect RTK dependency information
        - `rtk gain` - estimate output savings

        ## Rewrite Mode

        When rewrite hooks are installed, common commands are transformed automatically. The active installed RTK version reports these rewrite-capable commands:

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
          if (is_ignored[cmd]) next;

          desc = substr($0, index($0, $2));
          gsub(/^[ \t]+/, "", desc);
          gsub(/[ \t]+/, " ", desc);

          printf "- `rtk %s <args>` — %s (replaces `%s`)\n", cmd, desc, cmd;
        }
        ' >> $out/rtk/SKILL.md

        cat >> $out/rtk/SKILL.md <<'FOOTER'

        Do not rely on memory for this list. Check `rtk --help` or `rtk rewrite` for the active environment when behavior matters.

        ## Debugging

        1. Run `rtk rewrite '<command>'` to inspect rewrite output.
        2. If rewritten output is inappropriate, retry with `command <original>` to bypass shell/function resolution.
        3. If command permissions are involved, remember some harnesses evaluate the rewritten command rather than the original.
        4. For local configuration, inspect the project's/Home Manager's RTK config before changing global instructions.
        FOOTER
      '';

  rtkInstructionText = lib.strings.trim ''
    - RTK may automatically rewrite shell commands for compact output; to call non-rewrite commands, use `command <command>`.
    - Read the `rtk` skill for rewrite behavior, debugging, and direct RTK commands.
  '';

  rtkRewriteClaudeSh = pkgs.writeShellApplication {
    name = "rtk-rewrite-claude";
    excludeShellChecks = [ "SC2016" ];
    runtimeInputs = [
      pkgs.jaq
      pkgs.unstable.rtk
    ];
    text = builtins.readFile ./rtk-rewrite-claude.sh;
  };

  rtkRewriteOpencodeTs = builtins.replaceStrings [ "__RTK_BIN__" ] [ rtkBin ] (
    builtins.readFile ./rtk-rewrite-opencode.ts
  );

  rtkConfig = {
    hooks = {
      exclude_commands = [
        "cat"
        "curl"
        "find"
        "git"
        "grep"
        "rg"
      ];
    };
  };
in
{
  home.packages = [ pkgs.unstable.rtk ];

  home.file = {
    "Library/Application Support/rtk/config.toml" = lib.mkIf pkgs.stdenv.isDarwin {
      source = tomlFormat.generate "rtk-config" rtkConfig;
    };
    ".pi/agent/extensions/hm-rtk-rewrite/index.ts" = lib.mkIf piCfg.enable {
      text = builtins.replaceStrings [ "\"__RTK_BIN__\"" ] [ "\"${rtkBin}\"" ] (
        builtins.readFile ./rtk-rewrite-pi.ts
      );
    };
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

  agents.skillSets.rtk = rtkSkillSet;
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
