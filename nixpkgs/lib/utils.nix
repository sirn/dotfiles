{
  nixosOr = { nixosBin, otherBin }:
    if config.machine.nixos.enable
    then nixosBin
    else otherBin;
}
