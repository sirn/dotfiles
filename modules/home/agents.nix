{ config, lib, pkgs, ... }:

let
  sharedAgents = {
    quality-reviewer = {
      description = "Expert code quality reviewer focusing on bugs and logic";
      claude-code = {
        allowedTools = [
          "Read"
          "Grep"
          "Glob"
          "WebSearch"
          "WebFetch"
          "mcp__context7__resolve-library-id"
          "mcp__context7__query-docs"
        ];
        color = "red";
        model = "sonnet";
      };
      opencode = {
        model = "google/gemini-3-pro-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/quality-reviewer.md;
    };

    security-researcher = {
      description = "Specialist in threat modeling, vulnerability research, and secure design";
      claude-code = {
        allowedTools = [
          "Read"
          "Grep"
          "Glob"
          "WebSearch"
          "WebFetch"
          "mcp__context7__resolve-library-id"
          "mcp__context7__query-docs"
        ];
        color = "yellow";
        model = "opus";
      };
      opencode = {
        model = "google/gemini-3-pro-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/security-researcher.md;
    };

    convention-reviewer = {
      description = "Meticulous reviewer for coding conventions and consistency";
      claude-code = {
        allowedTools = [ "Read" "Grep" "Glob" ];
        color = "blue";
        model = "sonnet";
      };
      opencode = {
        model = "google/gemini-3-flash-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/convention-reviewer.md;
    };

    simplicity-reviewer = {
      description = "Pragmatic reviewer prioritizing simplicity over abstraction";
      claude-code = {
        allowedTools = [ "Read" "Grep" "Glob" ];
        color = "green";
        model = "sonnet";
      };
      opencode = {
        model = "google/gemini-3-flash-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/simplicity-reviewer.md;
    };

    doc-researcher = {
      description = "Finds authoritative documentation and API references";
      claude-code = {
        allowedTools = [
          "Read"
          "Grep"
          "Glob"
          "WebSearch"
          "WebFetch"
          "mcp__context7__resolve-library-id"
          "mcp__context7__query-docs"
        ];
        color = "purple";
        model = "sonnet";
      };
      opencode = {
        model = "google/gemini-3-flash-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/doc-researcher.md;
    };

    best-practices-researcher = {
      description = "Expert at researching best practices and patterns via web search";
      claude-code = {
        allowedTools = [
          "Read"
          "Grep"
          "Glob"
          "WebSearch"
          "WebFetch"
          "mcp__context7__resolve-library-id"
          "mcp__context7__query-docs"
        ];
        color = "purple";
        model = "sonnet";
      };
      opencode = {
        model = "google/gemini-3-flash-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/best-practices-researcher.md;
    };

    troubleshooter = {
      description = "Debugs issues by researching errors, logs, and known fixes";
      claude-code = {
        allowedTools = [
          "Read"
          "Grep"
          "Glob"
          "WebSearch"
          "WebFetch"
          "mcp__context7__resolve-library-id"
          "mcp__context7__query-docs"
        ];
        color = "red";
        model = "sonnet";
      };
      opencode = {
        model = "google/gemini-3-flash-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/troubleshooter.md;
    };

    code-architect = {
      description = "Analyzes architecture and provides design guidance";
      claude-code = {
        allowedTools = [
          "Read"
          "Grep"
          "Glob"
          "WebSearch"
          "WebFetch"
          "mcp__context7__resolve-library-id"
          "mcp__context7__query-docs"
        ];
        color = "orange";
        model = "opus";
      };
      opencode = {
        model = "google/gemini-3-pro-preview";
      };
      prompt = builtins.readFile ../../var/agents/agents/code-architect.md;
    };
  };

  # Claude Code uses tools from agent.tools; opencode doesn't support tool restrictions, colors, or model
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

  mkOpencodeAgent = name: agent: ''
    ---
    name: ${name}
    description: ${agent.description}
    model: ${agent.opencode.model}
    mode: subagent
    ---
    ${agent.prompt}
  '';

  claudeCodeCfg = config.programs.claude-code;
  opencodeCfg = config.programs.opencode;
in
{
  imports = [
    ../programs/claude-code.nix
    ../programs/codex.nix
    ../programs/gemini.nix
    ../programs/mcp.nix
    ../programs/opencode.nix
  ];

  programs = {
    git.ignores = [ ".my/" ];

    claude-code = lib.mkIf claudeCodeCfg.enable {
      agents = lib.mapAttrs mkClaudeCodeAgent sharedAgents;
    };

    opencode = lib.mkIf opencodeCfg.enable {
      agents = lib.mapAttrs mkOpencodeAgent sharedAgents;
    };
  };
}
