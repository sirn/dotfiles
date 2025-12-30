{ lib, pkgs, uv2nix, pyproject-nix, pyproject-build-systems }:

let
  python = pkgs.python312;

  workspace = uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = ./uv2nix;
  };

  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };

  pythonSet = (pkgs.callPackage pyproject-nix.build.packages { inherit python; }).overrideScope (
    lib.composeManyExtensions [
      pyproject-build-systems.overlays.wheel
      overlay
    ]
  );

  venv = pythonSet.mkVirtualEnv "mcp-server-fetch" workspace.deps.default;
in
venv.overrideAttrs (old: {
  name = "mcp-server-fetch-2025.4.7";
  meta = (old.meta or { }) // {
    mainProgram = "mcp-server-fetch";
  };
  passthru.updateScript = ./update.sh;
})
