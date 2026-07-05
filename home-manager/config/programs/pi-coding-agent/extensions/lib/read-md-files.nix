# Load every *.md file under `dir` as home.file text entries keyed `${prefix}/${name}`.
# `exclude` drops specific filenames (e.g. prompt.md when it is handled separately).
{ lib }:
{
  dir,
  prefix,
  exclude ? [ ],
}:
builtins.listToAttrs (
  builtins.map
    (name: lib.nameValuePair "${prefix}/${name}" { text = builtins.readFile (dir + "/${name}"); })
    (
      builtins.filter (n: builtins.match ".*\\.md$" n != null && !builtins.elem n exclude) (
        builtins.attrNames (builtins.readDir dir)
      )
    )
)
