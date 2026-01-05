nix-mode nix-ts-mode

(mkmodule "Nix Module Template"
 "{ config, lib, pkgs, ... }:" n n
 "{" n>
 (p "option") " = {" n>
 (p "enable") " = " (p "true") ";" n>
 q n>
 "};" n
 "}")
