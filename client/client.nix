{ mkDerivation, base, common, http-client, mtl, servant
, servant-client, stdenv
}:
mkDerivation {
  pname = "client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base common http-client mtl servant servant-client
  ];
  license = stdenv.lib.licenses.bsd3;
}
