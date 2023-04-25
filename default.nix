{ mkDerivation, base, containers, data-elevator, deepseq, HUnit
, lib
}:
mkDerivation {
  pname = "data-forced";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base data-elevator deepseq ];
  testHaskellDepends = [ base containers HUnit ];
  homepage = "https://github.com/RubenAstudillo/data-forced";
  description = "Specify that lifted values were forced to WHNF or NF";
  license = lib.licenses.mit;
}
