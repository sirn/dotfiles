{ lib, ... }:

{
  agents.models = lib.mkDefault {
    default = { };
    providers = { };
  };
}
