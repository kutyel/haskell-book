{ mkDerivation, base, checkers, cond, generic-lens, hpack, hspec
, QuickCheck, random, split, stdenv, time
}:
mkDerivation {
  pname = "haskell-book";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cond generic-lens random split time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base cond generic-lens random split time
  ];
  testHaskellDepends = [
    base checkers cond generic-lens hspec QuickCheck random split time
  ];
  prePatch = "hpack";
  homepage = "https://github.com/kutyel/haskell-book#readme";
  license = stdenv.lib.licenses.bsd3;
}
