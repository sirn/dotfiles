{
  fetchFromGitHub,
  ast-grep,
}:

let
  # Agent skills are published separately from the CLI in the
  # ast-grep/agent-skill repository. nixpkgs' ast-grep derivation installs
  # only the binary, so include the skills in the same output here.
  agentSkills = fetchFromGitHub {
    owner = "ast-grep";
    repo = "agent-skill";
    rev = "c2a9bc154f4ffe08b25d28d5e852dfac8c0d0d8a";
    hash = "sha256-awochSE2OupbsmaGx0xc7wHf0ovVMSdtHv4gZAGWOus=";
  };
in

ast-grep.overrideAttrs (old: {
  passthru = (old.passthru or { }) // {
    inherit agentSkills;
  };

  # Copy skills away from the read-only store so their documentation can be
  # tied to the actual CLI version.
  postPatch = (old.postPatch or "") + ''
    install -d "$TMPDIR/agent-skills"
    cp -r ${agentSkills}/ast-grep/skills/ast-grep "$TMPDIR/agent-skills/"
    cp -r ${agentSkills}/ast-grep/skills/outline "$TMPDIR/agent-skills/"

    substituteInPlace "$TMPDIR/agent-skills/ast-grep/SKILL.md" \
      --replace-fail \
      "# ast-grep Code Search" \
      "# ast-grep Code Search (${old.version})"

    substituteInPlace \
      "$TMPDIR/agent-skills/ast-grep/references/rule_reference.md" \
      --replace-fail \
      "Use \`dump_syntax_tree\` to see the actual AST structure" \
      "Use \`--debug-query=cst\` to see the actual AST structure"
  '';

  postInstall = (old.postInstall or "") + ''
    install -d "$out/skills"
    cp -r "$TMPDIR/agent-skills/ast-grep" "$out/skills/"
    cp -r "$TMPDIR/agent-skills/outline" "$out/skills/"
  '';
})
