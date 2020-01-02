{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  main = (import ./default.nix);
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = main.project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
  ];
}
