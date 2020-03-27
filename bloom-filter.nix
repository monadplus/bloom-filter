{ mkDerivation, base, bytestring, containers, directory, filepath
, mtl, stdenv, text, hspec, QuickCheck, data-hash, hashable
, vector, bitarray
}:
mkDerivation {
  pname = "bloom-filter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base QuickCheck containers
  ];
  libraryHaskellDepends = [
    bytestring containers directory filepath mtl text data-hash
    vector bitarray
  ];
  testHaskellDepends = [
    base hspec QuickCheck
  ];
  doHaddock = false;
  description = "";
  license = stdenv.lib.licenses.mit;
}
