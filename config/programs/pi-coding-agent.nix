{
  config,
  lib,
  pkgs,
  ...
}:

let
  instructionText = builtins.readFile ../../var/agents/instruction.md;
  skillsDir = ../../var/agents/skills;
  permissionsToml = lib.importTOML ../../var/agents/permissions.toml;

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -- "${lib.getExe pkgs.local.pi-coding-agent}" "$@"
  '';

  settingsJson = builtins.toJSON {
    quietStartup = true;
    defaultProvider = "fireworks";
    defaultModel = "accounts/fireworks/models/kimi-k2p5";
    defaultThinkingLevel = "medium";
    hideThinkingBlock = true;
    enabledModels = [
      "accounts/fireworks/models/*"
      "hf:zai-org/*"
      "hf:moonshotai/*"
      "hf:MiniMaxAI/*"
      "claude-opus-4-6"
      "claude-sonnet-4-6"
      "gpt-5.3-codex"
      "gpt-5.3-codex-spark"
      "gemini-3.1-pro-preview"
      "gemini-3-flash-preview"
    ];
    skills = [
      "skills/home-manager"
    ]
    ++ (map (name: "skills/mcp/mcp-${name}") (builtins.attrNames config.programs.mcp.servers));
    extensions = [
      "extensions/safety-gate.ts"
    ];
  };

  modelsJson = builtins.toJSON {
    providers = {
      synthetic = {
        baseUrl = "https://api.synthetic.new/openai/v1";
        apiKey = "SYNTHETIC_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "hf:moonshotai/Kimi-K2.5";
            name = "Kimi K2.5 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "hf:zai-org/GLM-4.7";
            name = "GLM 4.7 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "hf:MiniMaxAI/MiniMax-M2.5";
            name = "MiniMax M2.5 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1000000;
            maxTokens = 32768;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
        ];
      };
      fireworks = {
        baseUrl = "https://api.fireworks.ai/inference/v1";
        apiKey = "FIREWORKS_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "accounts/fireworks/models/kimi-k2p5";
            name = "Kimi K2.5 (Fireworks)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            cost = {
              input = 0.6;
              output = 3.0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "accounts/fireworks/models/glm-5";
            name = "GLM 5 (Fireworks)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            cost = {
              input = 1.0;
              output = 3.2;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
        ];
      };
    };
  };

  agentsMdText = ''
    ${instructionText}

    ## MCP Skills (Pi-specific)
    - MCP skills are located in `~/.pi/agent/skills/mcp/mcp-<name>/` directories.
    - Each MCP skill provides a `./mcp-wrapper.sh` script for tool discovery and execution.
    - **ALWAYS run `./mcp-wrapper.sh list` first to discover available tools before attempting to call any tool.**
    - After discovering tools, execute with: `./mcp-wrapper.sh call <tool-name> '<json-arguments>'`

    ## Safety Guidelines (Pi-specific)
    - When running destructive commands (`rm`, etc.), you must first ask the user.
    - When doing a commit, ask user for confirmation first.
    - Do not squash commit unless being told explicitly by the user.
  '';

  isStdioServer = server: server ? command || server ? package;

  toPiMcpWrapperScript =
    name: server:
    let
      isStdio = isStdioServer server;
      serverCmd = if isStdio then (server.command or (lib.getExe server.package)) else "";
      serverUrl = if isStdio then "" else server.url;
      jqBin = lib.getExe pkgs.jq;
    in
    pkgs.writeShellScript "mcp-${name}-wrapper" (
      if isStdio then
        ''
          # MCP: ${name}
          # Transport: stdio

          COMMAND=$1

          case "$COMMAND" in
            list)
              REQUEST='{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
              ;;
            call)
              TOOL=$2
              ARGS="''${3:-'{}'}"
              REQUEST=$(${jqBin} -n -c \
                --arg tool "$TOOL" \
                --argjson args "$ARGS" \
                '{jsonrpc:"2.0",id:1,method:"tools/call",params:{name:$tool,arguments:$args}}')
              ;;
            *)
              echo "Usage: $0 list"
              echo "       $0 call <tool> '<json-args>'"
              exit 1
              ;;
          esac

          echo "$REQUEST" | ${serverCmd} | ${jqBin} -r '.result | if (.tools) then (.tools[] | .name) else (.content[] | .text) end'
        ''
      else
        ''
          # MCP: ${name}
          # Transport: sse

          COMMAND=$1

          case "$COMMAND" in
            list)
              REQUEST='{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
              ;;
            call)
              TOOL=$2
              ARGS="''${3:-'{}'}"
              REQUEST=$(${jqBin} -n -c \
                --arg tool "$TOOL" \
                --argjson args "$ARGS" \
                '{jsonrpc:"2.0",id:1,method:"tools/call",params:{name:$tool,arguments:$args}}')
              ;;
            *)
              echo "Usage: $0 list"
              echo "       $0 call <tool> '<json-args>'"
              exit 1
              ;;
          esac

          # Use a FIFO to maintain persistent stdin for SSE transport
          # mcp-remote requires an open connection to receive async responses
          FIFO=$(mktemp -u)
          mkfifo "$FIFO"
          # Open FIFO read-write (doesn't block on Linux) to keep it open via FD 3
          exec 3<> "$FIFO"
          echo "$REQUEST" > "$FIFO"
          ${pkgs.coreutils}/bin/timeout "''${TIMEOUT:-30}" ${lib.getExe pkgs.local.mcpServers.mcp-remote} "${serverUrl}" < "$FIFO" 2>/dev/null | ${jqBin} -r '.result | if (.tools) then (.tools[] | .name) else (.content[] | .text) end'
          exec 3<&-
          rm -f "$FIFO"
        ''
    );

  toPiMcpSkillText = name: ''
    ---
    name: mcp-${name}
    description: Skill to call ${name} MCP server. Run ./mcp-wrapper.sh list to discover available tools, then ./mcp-wrapper.sh call <tool-name> '<json-arguments>'.
    ---

    ## Tool discovery
    ./mcp-wrapper.sh list

    ## Tool execution
    ./mcp-wrapper.sh call <tool-name> '<json-arguments>'

    ## Example
    ./mcp-wrapper.sh call search '{"query": "example"}'
  '';

  mkMcpSkillFiles = name: server: {
    ".pi/agent/skills/mcp/mcp-${name}/SKILL.md".text = toPiMcpSkillText name;
    ".pi/agent/skills/mcp/mcp-${name}/mcp-wrapper.sh".source = toPiMcpWrapperScript name server;
  };

  mcpSkillFiles = lib.mkMerge (lib.mapAttrsToList mkMcpSkillFiles config.programs.mcp.servers);

  # Generate safety-gate.ts from permissions.toml
  toRegexPattern = cmd:
    let
      # Escape special regex characters in the command
      escaped = lib.concatMapStrings (c:
        if c == "\\" then "\\\\"
        else if c == "." then "\\."
        else if c == "*" then "\\*"
        else if c == "+" then "\\+"
        else if c == "?" then "\\?"
        else if c == "^" then "\\^"
        else if c == "$" then "\\$"
        else if c == "(" then "\\("
        else if c == ")" then "\\)"
        else if c == "[" then "\\["
        else if c == "]" then "\\]"
        else if c == "{" then "\\{"
        else if c == "}" then "\\}"
        else if c == "|" then "\\|"
        else c
      ) (lib.stringToCharacters cmd);
    in "/^\\s*${escaped}\\b/i";

  askPatterns = lib.concatMapStringsSep ",\n  " toRegexPattern permissionsToml.default.commands.ask.shell;
  denyPatterns = lib.concatMapStringsSep ",\n  " toRegexPattern permissionsToml.default.commands.deny.shell;

  safetyGateTs = ''
/**
 * Safety Gate Extension for Pi Coding Agent
 *
 * Generated from permissions.toml
 * - ask: commands requiring user confirmation
 * - deny: commands that are blocked entirely
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

// Commands that require user confirmation (from permissions.toml [default.commands.ask])
const askPatterns = [
  ${askPatterns}
];

// Commands that are denied entirely (from permissions.toml [default.commands.deny])
const denyPatterns = [
  ${denyPatterns}
];

// Check if a command matches any pattern
function matchesPattern(command: string, patterns: RegExp[]): boolean {
  return patterns.some((pattern) => pattern.test(command));
}

// Extract the base command for display
function getCommandSummary(command: string): string {
  if (command.length > 80) {
    return command.slice(0, 77) + "...";
  }
  return command;
}

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return undefined;

    const command = event.input.command as string;

    // Check deny patterns first (highest priority)
    if (matchesPattern(command, denyPatterns)) {
      return {
        block: true,
        reason: `Command blocked by safety policy: "''${getCommandSummary(command)}"`,
      };
    }

    // Check ask patterns - require confirmation
    if (matchesPattern(command, askPatterns)) {
      if (!ctx.hasUI) {
        return {
          block: true,
          reason: `Command blocked (no UI for confirmation): "''${getCommandSummary(command)}"`,
        };
      }

      const choice = await ctx.ui.select(
        `Confirm: ''${getCommandSummary(command)}`,
        ["Yes, proceed", "No, cancel"]
      );

      if (choice !== "Yes, proceed") {
        ctx.ui.notify("Command cancelled by user", "info");
        return { block: true, reason: "Blocked by user" };
      }
    }

    return undefined;
  });
}
'';

  baseFiles = {
    ".pi/agent/settings.json".text = settingsJson;
    ".pi/agent/models.json".text = modelsJson;
    ".pi/agent/AGENTS.md".text = agentsMdText;
    ".pi/agent/skills/home-manager".source = skillsDir;
    ".pi/agent/extensions/safety-gate.ts".text = safetyGateTs;
  };
in
{
  home.packages = [ wrappedPi ];

  programs.git.ignores = [ ".pi/" ];

  home.file = lib.mkMerge [
    baseFiles
    mcpSkillFiles
  ];
}
