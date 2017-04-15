{ mkDerivation, base, common, dependent-sum, hashable, mtl
, primitive, ref-tf, reflex, servant, servant-snap
, servant-snap-reflex, snap, snap-core, snap-server, stdenv, stm
, ttrie
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base common dependent-sum hashable mtl primitive ref-tf reflex
    servant servant-snap servant-snap-reflex snap snap-core snap-server
    stm ttrie
  ];
  license = stdenv.lib.licenses.bsd3;
}
