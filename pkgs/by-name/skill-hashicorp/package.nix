{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in

stdenvNoCC.mkDerivation {
  pname = "skill-hashicorp";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "hashicorp";
    repo = "agent-skills";
    inherit (sources) rev;
    hash = sources.srcHash;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/skills
    cp -r plugins/terraform/skills $out/skills/terraform
    cp -r plugins/packer/skills $out/skills/packer
    chmod +x $out/skills/terraform/terraform-search-import/scripts/list_resources.sh

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "Official HashiCorp agent skills for Terraform and Packer";
    homepage = "https://github.com/hashicorp/agent-skills";
    license = lib.licenses.mpl20;
  };
}
