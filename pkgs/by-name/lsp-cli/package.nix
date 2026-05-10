{
  lib,
  stdenvNoCC,
  python3,
  makeWrapper,
  clang-tools,
  gopls,
  typescript-language-server,
  nixd,
  pyright,
  rust-analyzer,
  bash-language-server,
  yaml-language-server,
  intelephense,
}:

let
  pythonEnv = python3.withPackages (ps: [ ]);

  lspPath = lib.makeBinPath [
    clang-tools # provides clangd
    gopls
    typescript-language-server
    nixd
    pyright
    rust-analyzer
    bash-language-server
    yaml-language-server
    intelephense
  ];
in
stdenvNoCC.mkDerivation {
  pname = "lsp-cli";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    install -Dm644 lsp.py $out/libexec/lsp-cli/lsp.py

    makeWrapper ${pythonEnv}/bin/python3 $out/bin/lsp \
      --add-flags "$out/libexec/lsp-cli/lsp.py" \
      --prefix PATH : ${lspPath}

    mkdir -p $out/skills
    cp -r skills/. $out/skills/

    runHook postInstall
  '';

  meta = {
    description = "LSP client CLI for agents — code intelligence via Language Server Protocol";
    license = lib.licenses.mit;
    mainProgram = "lsp";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
