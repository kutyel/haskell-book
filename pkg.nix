{ mkDerivation, base, checkers, cond, generic-lens, hpack, hspec
, parsers, QuickCheck, random, split, stdenv, time, trifecta
}:
mkDerivation {
  pname = "haskell-book";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cond generic-lens parsers random split time trifecta
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base cond generic-lens parsers random split time trifecta
  ];
  testHaskellDepends = [
    base checkers cond generic-lens hspec QuickCheck random split time
  ];
  prePatch = "hpack";
  homepage = "https://github.com/kutyel/haskell-book#readme";
  license = stdenv.lib.licenses.bsd3;
}
