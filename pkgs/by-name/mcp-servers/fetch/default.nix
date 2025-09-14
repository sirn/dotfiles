{ lib, pkgs }:

pkgs.python3Packages.buildPythonPackage rec {
  pname = "mcp-server-fetch";
  version = "2025.4.7";

  src = pkgs.fetchurl {
    url = "https://files.pythonhosted.org/packages/02/0f/718dde44fd8b8d2792ae6011a3d2c3080440af48cee5d2ee8b7941b75e8e/mcp_server_fetch-2025.4.7.tar.gz";
    hash = "sha256-VieePFXLHlBrlYypuyPtRBOUSm8jC8oh4ESu5Rc0/kc=";
  };

  format = "pyproject";

  nativeBuildInputs = with pkgs.python3Packages; [
    hatchling
  ];

  propagatedBuildInputs = with pkgs.python3Packages; [
    mcp
    httpx
    markdownify
    protego
    readabilipy
    requests
  ];

  pythonRelaxDeps = true;

  meta = {
    description = "MCP server for fetching web content";
    homepage = "https://github.com/modelcontextprotocol/servers";
    downloadPage = "https://pypi.org/project/mcp-server-fetch/";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "mcp-server-fetch";
  };
}