{ pkgs ? import ./nixpkgs.nix
, compiler ? "ghc883"
}:

let

fontsConf = pkgs.makeFontsConf {
  fontDirectories = [ pkgs.cantarell-fonts ];
};

project = import ./. { inherit compiler pkgs; };

in

pkgs.haskell.packages.${compiler}.shellFor {
  withHoogle = true;
  packages = p: [ project.concurrency ];
  buildInputs = [ pkgs.cabal-install ];
  FONTCONFIG_FILE = fontsConf;
}
