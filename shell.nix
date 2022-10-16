{ ... }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.Cabal_3_6_3_0
    haskell.compiler.ghc8107
    cabal-install
    ormolu
  ];
}
