{ mkDerivation, base, data-elevator, lib }:
mkDerivation {
  pname = "data-elevator-forced";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base data-elevator ];
  testHaskellDepends = [ base ];
  description = "Specify that lifted values were forces to WHNF or NF";
  license = lib.licenses.mit;
}
