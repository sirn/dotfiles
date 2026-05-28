{
  config,
  lib,
  pkgs,
  ...
}:

let
  skillSetType = lib.types.coercedTo lib.types.path (path: { inherit path; }) (
    lib.types.submodule {
      options = {
        path = lib.mkOption {
          type = lib.types.path;
          description = "Directory whose immediate child directories are agent skills.";
        };

        prefix = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Optional prefix to add to discovered skill directory names.";
        };

        skills = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Optional allow-list of source skill directory names to discover from this skill set. Empty means discover all skills.";
        };
      };
    }
  );
  rewriteSkillFrontmatter =
    skill: source:
    if !skill.rewriteFrontmatter then
      source
    else
      pkgs.runCommand "${skill.name}-SKILL.md"
        {
          inherit source;
          renderedName = skill.name;
        }
        ''
          awk -v renderedName="$renderedName" '
            BEGIN { inFrontmatter = 0; replaced = 0 }
            NR == 1 && $0 == "---" { inFrontmatter = 1; print; next }
            inFrontmatter && $0 == "---" {
              if (!replaced) print "name: " renderedName
              inFrontmatter = 0
              print
              next
            }
            inFrontmatter && $0 ~ /^name:[[:space:]]*/ && !replaced {
              print "name: " renderedName
              replaced = 1
              next
            }
            { print }
          ' "$source" > "$out"
        '';

  discoverSkillSet =
    setName: skillSet:
    let
      entries = builtins.readDir skillSet.path;
      renderName = name: if skillSet.prefix == null then name else "${skillSet.prefix}-${name}";
      isIncluded = name: skillSet.skills == [ ] || builtins.elem name skillSet.skills;
      isSkillDir =
        name: type:
        let
          source = skillSet.path + "/${name}";
        in
        isIncluded name
        && (type == "directory" || type == "symlink")
        && builtins.pathExists (source + "/SKILL.md");
    in
    map (sourceName: {
      name = renderName sourceName;
      inherit setName sourceName;
      source = skillSet.path + "/${sourceName}";
      rewriteFrontmatter = skillSet.prefix != null;
    }) (builtins.attrNames (lib.filterAttrs isSkillDir entries));

  discoveredSkills = lib.flatten (lib.mapAttrsToList discoverSkillSet config.agents.skillSets);

  skillNames = map (skill: skill.name) discoveredSkills;
  duplicateSkillNames = lib.filter (name: (lib.count (n: n == name) skillNames) > 1) (
    lib.unique skillNames
  );

  renderSkillTree =
    name:
    let
      skillLinks = lib.concatMap (
        skill:
        let
          entries = builtins.readDir skill.source;
          visibleEntries = lib.filterAttrs (entryName: _: entryName != "SKILL.md") entries;
          entryLinks = lib.mapAttrsToList (entryName: _: {
            name = "${skill.name}/${entryName}";
            path = skill.source + "/${entryName}";
          }) visibleEntries;
        in
        [
          {
            name = "${skill.name}/SKILL.md";
            path = rewriteSkillFrontmatter skill (skill.source + "/SKILL.md");
          }
        ]
        ++ entryLinks
      ) discoveredSkills;
    in
    pkgs.linkFarm name skillLinks;
in
{
  imports = [
    ./domains.nix
    ./models.nix
    ./permissions.nix
  ];

  options.agents = {
    instructionText = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Shared instruction text (AGENTS.md) for all agents.";
    };

    subagentPreamble = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Preamble prepended to every subagent persona to override orchestration instructions.";
    };

    commandContext = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = ''
        Safety and behavioral descriptions for allowed commands.
        Each contribution describes what a command does, whether it is
        read-only, and any side effects it may have.
      '';
    };

    skillSets = lib.mkOption {
      type = lib.types.attrsOf skillSetType;
      default = { };
      description = "Named directories whose immediate child directories are agent skills.";
    };

    discoveredSkills = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption { type = lib.types.str; };
            sourceName = lib.mkOption { type = lib.types.str; };
            setName = lib.mkOption { type = lib.types.str; };
            source = lib.mkOption { type = lib.types.path; };
            rewriteFrontmatter = lib.mkOption { type = lib.types.bool; };
          };
        }
      );
      readOnly = true;
      description = "Flattened list of discovered agent skill directories.";
    };

    skillTrees = lib.mkOption {
      type = lib.types.path;
      readOnly = true;
      description = "Rendered skill tree.";
    };
  };

  config = {
    assertions = [
      {
        assertion = duplicateSkillNames == [ ];
        message = "Duplicate agent skill names: ${lib.concatStringsSep ", " duplicateSkillNames}";
      }
    ];

    agents.discoveredSkills = discoveredSkills;
    agents.skillTrees = renderSkillTree "agent-skills";

  };
}
