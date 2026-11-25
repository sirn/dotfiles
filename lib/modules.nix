{ lib }:
let
  # Directories without a default.nix become nested attrsets. A directory with
  # a default.nix collapses to its own path, so `import` works on the value.
  scan =
    dir:
    let
      entries = lib.filterAttrs (
        name: type: type == "directory" || (type == "regular" && lib.hasSuffix ".nix" name)
      ) (builtins.readDir dir);
      value =
        name: type:
        let
          path = dir + "/${name}";
        in
        if type == "directory" then
          if builtins.pathExists (path + "/default.nix") then path else scan path
        else
          path;
    in
    lib.mapAttrs' (
      name: type:
      lib.nameValuePair (if type == "directory" then name else lib.removeSuffix ".nix" name) (
        value name type
      )
    ) entries;
in
{
  nixos = scan ../nixos;
  homeManager = scan ../home-manager;
}
