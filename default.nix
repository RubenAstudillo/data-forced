{ mkDerivation, base, containers, data-elevator, deepseq, HUnit
, lib, nothunks
}:
mkDerivation {
  pname = "data-elevator-forced";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base data-elevator deepseq nothunks ];
  testHaskellDepends = [
    base containers data-elevator HUnit nothunks
  ];
  description = "Specify that lifted values were forces to WHNF or NF";
  license = lib.licenses.mit;
}
