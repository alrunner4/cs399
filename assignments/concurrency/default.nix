{ pkgs ? import ./nixpkgs.nix
, compiler ? "ghc883"
}:{
  concurrency = import ./concurrency.nix { inherit compiler pkgs; };
}
