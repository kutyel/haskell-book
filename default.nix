let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./pkg.nix { }
