let
  pkgs = import <nixpkgs> { };
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
 in {
  project = pkgs.haskellPackages.callPackage ./pkg.nix { };
  hpc-coveralls = pkgs.haskellPackages.callPackage ./hpc-coveralls.nix { };
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
