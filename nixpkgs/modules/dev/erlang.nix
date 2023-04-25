{ config, pkgs, ... }:

{
  home.packages = with pkgs.beam.packages.erlangR24; [
    elixir_1_14
    elixir-ls
    erlang
    erlang-ls
    rebar3
  ];
}
