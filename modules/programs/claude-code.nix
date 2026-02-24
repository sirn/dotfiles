{ config, lib, pkgs, ... }:

let
  cfg = config.programs.claude-code;

  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  agentsDir = ../../var/agents/agents;

  agentFiles = builtins.filter
    (name: lib.hasSuffix ".toml" name)
    (builtins.attrNames (builtins.readDir agentsDir));

  loadAgent = tomlFile:
    let
      name = lib.removeSuffix ".toml" tomlFile;
      agentConfig = builtins.fromTOML (builtins.readFile (agentsDir + "/${tomlFile}"));
      prompt = builtins.readFile (agentsDir + "/${name}.md");
    in
    {
      inherit name;
      value = agentConfig // { inherit prompt; };
    };

  agents = builtins.listToAttrs (map loadAgent agentFiles);

  # Filter agents that have claude-code configuration
  claudeCodeAgents = lib.filterAttrs (name: agent: agent ? claude-code) agents;

  validateClaudeCodeAgent = name: agent:
    let
      valid =
        if !(agent ? description) then throw "Agent ${name}: missing 'description'"
        else if !(agent.claude-code ? allowedTools) then throw "Agent ${name}: missing 'claude-code.allowedTools'"
        else if !(agent.claude-code ? color) then throw "Agent ${name}: missing 'claude-code.color'"
        else if !(agent.claude-code ? model) then throw "Agent ${name}: missing 'claude-code.model'"
        else agent;
    in valid;

  validClaudeCodeAgents = lib.mapAttrs validateClaudeCodeAgent claudeCodeAgents;

  mkClaudeCodeAgent = name: agent: ''
    ---
    name: ${name}
    description: ${agent.description}
    tools: ${lib.concatStringsSep ", " agent.claude-code.allowedTools}
    color: ${agent.claude-code.color}
    model: ${agent.claude-code.model}
    ---
    ${agent.prompt}
  '';

  isStdioServer = server: server ? command || server ? package;

  toClaudeCodeMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          type = "stdio";
          command = server.command or (lib.getExe server.package);
        } else {
          type = server.transport or "sse";
          url = server.url;
        })
      servers;

  statusLineScript = pkgs.writeShellScript "claude-statusline" ''
    input=$(cat)
    cwd=$(echo "$input" | ${lib.getExe pkgs.jq} -r '.workspace.current_dir')

    # Git/Jujutsu info
    vcs_info=""

    # Check for jj
    if command -v ${lib.getExe config.programs.jujutsu.package} &>/dev/null && ${lib.getExe config.programs.jujutsu.package} root --quiet 2>/dev/null; then
        jj_change=$(${lib.getExe config.programs.jujutsu.package} log --ignore-working-copy --no-graph -r @ -T 'separate("", change_id.shortest(), if(!empty, "*"))' 2>/dev/null)
        vcs_info=$(printf '\033[35m⎇ jj:%s\033[0m ' "$jj_change")
    fi

    # Check for git
    if [ -d "$cwd/.git" ] || ${lib.getExe pkgs.git} -C "$cwd" rev-parse --git-dir &>/dev/null; then
        branch=$(${lib.getExe pkgs.git} -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null || echo "detached")
        status=""
        if ! ${lib.getExe pkgs.git} -C "$cwd" --no-optional-locks diff-index --quiet HEAD -- 2>/dev/null; then
            status="*"
        fi
        vcs_info="''${vcs_info}$(printf '\033[35m⎇ git:%s%s\033[0m ' "$branch" "$status")"
    fi

    # Model info
    model=$(echo "$input" | ${lib.getExe pkgs.jq} -r '.model.id // .model.name // empty')
    model_info=""
    if [ -n "$model" ]; then
        model_info=$(printf '\033[34m◆ %s\033[0m ' "$model")
    fi

    # Cost info (only if cost > 0)
    total_cost=$(echo "$input" | ${lib.getExe pkgs.jq} -r '.cost.total_cost_usd // empty')
    cost_info=""
    if [ -n "$total_cost" ] && [ "$total_cost" != "0" ] && [ "$total_cost" != "0.0" ]; then
        # Round to 2 decimal places
        rounded_cost=$(printf "%.2f" "$total_cost")
        cost_info=$(printf '\033[37m▲ $%s\033[0m ' "$rounded_cost")
    fi

    # Session time (convert from milliseconds to days/hours/minutes/seconds)
    session_duration_ms=$(echo "$input" | ${lib.getExe pkgs.jq} -r '.cost.total_duration_ms // empty')
    session_info=""
    if [ -n "$session_duration_ms" ]; then
        session_duration=$((session_duration_ms / 1000))
        days=$((session_duration / 86400))
        hours=$(((session_duration % 86400) / 3600))
        minutes=$(((session_duration % 3600) / 60))
        seconds=$((session_duration % 60))

        # Build time string with only relevant units
        time_str=""
        [ "$days" -gt 0 ] && time_str="''${days}d "
        [ "$hours" -gt 0 ] && time_str="''${time_str}''${hours}h "
        time_str="''${time_str}''${minutes}m"

        session_info=$(printf '\033[36m◷ %s\033[0m ' "$time_str")
    fi

    # Context window info
    used_pct=$(echo "$input" | ${lib.getExe pkgs.jq} -r '.context_window.used_percentage // empty')
    context_info=""
    if [ -n "$used_pct" ]; then
        # Create visual bar (10 blocks total, each represents 10%)
        bar=$(${lib.getExe pkgs.gawk} -v pct="$used_pct" 'BEGIN {
            filled = int(pct / 10)
            empty = 10 - filled
            for (i = 0; i < filled; i++) printf "▮"
            for (i = 0; i < empty; i++) printf "▯"
        }')

        # Color based on usage: blue < 50%, yellow >= 50%, red >= 75%
        if [ "$used_pct" -ge 75 ]; then
            color='\033[31m'  # red
        elif [ "$used_pct" -ge 50 ]; then
            color='\033[33m'  # yellow
        else
            color='\033[34m'  # blue (same as model)
        fi

        context_info=$(printf "''${color}%s %s%%\033[0m " "$bar" "$used_pct")
    fi

    # Build status line
    printf '%s%s%s%s%s' \
        "$vcs_info" \
        "$model_info" \
        "$context_info" \
        "$cost_info" \
        "$session_info"
  '';
in
{
  programs.claude-code = {
    enable = true;
    package = pkgs.unstable.claude-code;

    agents = lib.mapAttrs mkClaudeCodeAgent validClaudeCodeAgents;
    memory.text = instructionText + ''

      ## Skill Execution (Subagent Enhancement)
      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents via the Task tool for higher-quality results.
    '';
    mcpServers = toClaudeCodeMcpServers config.programs.mcp.servers;

    settings = {
      model = "opusplan";
      includeCoAuthoredBy = false;
      cleanupPeriodDays = 7;
      statusLine = {
        type = "command";
        command = toString statusLineScript;
      };
      permissions = {
        allow = [
          "Read(**)"
          "Glob(*)"
          "Grep(*)"
          "Edit(**)"
          "Write(**)"
          "Bash(cat:*)"
          "Bash(find:*)"
          "Bash(fd:*)"
          "Bash(grep:*)"
          "Bash(rg:*)"
          "Bash(ls:*)"
          "Bash(curl:*)"
          "Bash(wget:*)"
          "Bash(git status:*)"
          "Bash(git diff:*)"
          "Bash(git log:*)"
          "Bash(git branch:*)"
          "Bash(jj status:*)"
          "Bash(jj diff:*)"
          "Bash(jj log:*)"
          "Bash(jj show:*)"
          "Bash(go test:*)"
          "Bash(go build:*)"
          "Bash(tree:*)"
          "Bash(lstr:*)"
          "Read(**/*.env.example)"
          "Read(**/*.env.sample)"
          "Write(**/*.env.example)"
          "Write(**/*.env.sample)"
          "WebSearch"
          "WebFetch(domain:*)"
        ];
        deny = [
          "Bash(sudo:*)"
          "Bash(doas:*)"
          "Bash(kill:*)"
          "Bash(systemctl:*)"
          "Bash(chown:*)"
          "Bash(sops:*)"
          "Bash(git push:*)"
          "Bash(jj git push:*)"
          "Read(**/.env)"
          "Read(**/.env.*)"
          "Read(**/*.env)"
          "Edit(**/.env)"
          "Edit(**/.env.*)"
          "Edit(**/*.env)"
          "Write(**/.env)"
          "Write(**/.env.*)"
          "Write(**/*.env)"
        ];
        ask = [
          "Bash(chmod:*)"
          "Bash(rm:*)"
          "Bash(git commit:*)"
          "Bash(jj git:*)"
          "Bash(jj describe:*)"
          "Bash(jj new:*)"
          "Bash(jj commit:*)"
          "Bash(jj squash:*)"
          "Bash(jj split:*)"
          "Bash(jj abandon:*)"
          "Bash(jj undo:*)"
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".claude/*.local.json"
      ".claude/*.local.md"
    ];
  };

  home.file.".claude/skills".source = lib.mkIf cfg.enable skillsDir;
}
