let
  pkgs = import <nixpkgs> { };
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/tarball/master");
 in {
  project = pkgs.haskellPackages.callPackage ./pkg.nix { };
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
