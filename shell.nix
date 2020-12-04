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
  shellHook = ''
    ${main.pre-commit-check.shellHook}
    ${main.hpc-coveralls} --repo-token=ny8sIrhwenvlvccqT6BKmzE0KZEtqKuZh haskell-book-test
  '';
}
