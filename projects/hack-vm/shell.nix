{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.haskell-language-server
  ];
}
