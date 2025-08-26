{ pkgs, ... }:

let
  fhsBun = pkgs.buildFHSEnv {
    name = "bun-fhs-base";
    runScript = "bun";
    targetPkgs = pkgs': with pkgs'; [
      bun
    ];
  };

  wrappedFhsBun = pkgs.stdenv.mkDerivation {
    pname = "wrapped-bun";
    src = ./.;
    version = pkgs.bun.version;

    nativeBuildInputs = [
      pkgs.makeWrapper
    ];

    installPhase = ''
      mkdir -p $out/bin

      makeWrapper ${fhsBun}/bin/${fhsBun.name} $out/bin/bun
      makeWrapper ${fhsBun}/bin/${fhsBun.name} $out/bin/bunx --add-flags "x"
    '';
  };

  actualBun =
    if pkgs.stdenv.isLinux
    then wrappedFhsBun
    else pkgs.bun;

  bunxOpenCode = pkgs.writeScriptBin "opencode" ''
    #!${pkgs.runtimeShell}
    # Runs OpenCode from Bunx
    export PATH=${actualBun}/bin:${pkgs.nodejs_20}/bin:${pkgs.local.wrapped-uv}/bin:$PATH
    exec ${actualBun}/bin/bun x opencode-ai@latest "$@"
  '';
in
{
  home.packages = with pkgs; [
    bunxOpenCode
  ];
}
