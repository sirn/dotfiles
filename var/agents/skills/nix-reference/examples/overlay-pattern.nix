let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (final: prev: {
        nodejs = prev.nodejs_22;
      })
    ];
  };
in { ... }
