{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.haskellPackages.Cabal_3_4_1_0
  ];

  shellHook = ''
    alias nv="nvim"
  '';
}
