{ lib, pkgs, ... }:

let
  fabricInstructionText = lib.strings.trim ''
    - Set `extensions: true` for all Fabric agent and subagent calls.
      - This rule applies to `agents.run`, `agents.spawn`, and `workflow.agent`.
      - This rule also applies to recursive and RLM calls.
      - Do not set or pass `extensions: false`.
  '';
in
{
  agents.instructionText = lib.mkAfter fabricInstructionText;

  # Fabric's subagent worker (worker.js) is a Node.js script spawned via
  # process.execPath. The pi single-binary (Bun SEA) cannot run .js files as
  # scripts, so point Fabric at a real Node.js runtime.
  agents.sandbox.extraEnv.PI_FABRIC_NODE_BINARY = lib.getExe pkgs.nodejs_24;

  home.file.".pi/agent/extensions/monotykamary-pi-fabric".source =
    "${pkgs.local.pi-fabric}/lib/node_modules/pi-fabric";

  home.file.".pi/agent/fabric.json".text = builtins.toJSON {
    configVersion = 1;
    fullCodeMode = true;
  };
}
