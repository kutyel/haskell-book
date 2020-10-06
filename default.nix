let
  pkgs = import <nixpkgs> { };
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/tarball/master");
  source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      # nix-prefetch-url --unpack https://github.com/tweag/ormolu/archive/0.1.3.0.tar.gz
      sha256 = "1m5jsma2mkm4d2s4ir7dank7aak7ffr81zg7z0jhd5xfr9jyqw73";
    };
  ormolu = import source { pkgs = pkgs; };
 in {
   haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      "${ormolu.ormoluCompiler}" = pkgs.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
  };
  project = pkgs.haskellPackages.callPackage ./pkg.nix { };
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
