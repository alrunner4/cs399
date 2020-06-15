{ pkgs ? import ./nixpkgs.nix
, compiler ? "ghc883"
}:{
  statemachine = import ./statemachine.nix { inherit compiler pkgs; };
}
