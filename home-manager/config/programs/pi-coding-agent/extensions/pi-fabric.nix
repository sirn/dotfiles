{ lib, pkgs, ... }:

let
  fabricInstructionText = lib.strings.trim ''
    - When calling Fabric agent/subagent entry points (including `agents.run`, `agents.spawn`, `workflow.agent`, and recursive/rlm calls where applicable), always set `extensions: true`; never set or pass `extensions: false`.
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
