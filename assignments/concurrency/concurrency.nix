{ pkgs ? import ./nixpkgs.nix
, compiler ? "ghc883"
}:
pkgs.haskell.packages.${compiler}.callCabal2nix "concurrency" ./. {}
