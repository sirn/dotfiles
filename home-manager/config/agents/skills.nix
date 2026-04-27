{ pkgs, ... }:

{
  agents.skillSets = {
    apis = ../../../var/agents/skills/apis;
    coding = ../../../var/agents/skills/coding;
    lifecycle = ../../../var/agents/skills/lifecycle;
    references = ../../../var/agents/skills/references;
    agent-stuff = {
      path = "${pkgs.local.mitsuhiko-agent-stuff}/skills";
      skills = [ "tmux" ];
    };
  };
}
