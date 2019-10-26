{ pkgs ? import <nixpkgs> { } }:

let
  easy-ps = import (fetchTarball
    "https://github.com/justinwoo/easy-purescript-nix/archive/aa94aeac3a6ad9b4dfa0e807ad1421097d74f663.tar.gz") {
      inherit pkgs;
    };
in pkgs.mkShell {
  buildInputs = [
    easy-ps.purs
    easy-ps.spago
    pkgs.gnumake
  ];
}
