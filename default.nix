{ mkDerivation, base, containers, data-elevator, deepseq, HUnit
, lib
}:
mkDerivation {
  pname = "data-elevator-forced";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base data-elevator deepseq ];
  testHaskellDepends = [ base containers HUnit ];
  description = "Specify that lifted values were forced to WHNF or NF";
  license = lib.licenses.mit;
}
