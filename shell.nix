{ pkgs ? import <nixpkgs> {} }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  myPkg = pkgs.haskell.packages.ghc912.callCabal2nix "aoc25" src {};
in
pkgs.stdenv.mkDerivation {
  name = "aoc-shell";

  buildInputs = [
    myPkg.env.nativeBuildInputs

    pkgs.cabal-install
    pkgs.haskell.packages.ghc912.haskell-language-server
    pkgs.hlint
  ];
}
