{ mkDerivation, base, checkers, cond, generic-lens, hspec
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
  executableHaskellDepends = [
    base cond parsers random split time trifecta
  ];
  testHaskellDepends = [
    base checkers cond hspec QuickCheck
  ];
  homepage = "https://github.com/kutyel/haskell-book#readme";
  license = stdenv.lib.licenses.bsd3;
}
