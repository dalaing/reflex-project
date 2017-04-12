{ mkDerivation, base, common, ghcjs-dom, reflex, reflex-dom, stdenv
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base common ghcjs-dom reflex reflex-dom
  ];
  license = stdenv.lib.licenses.bsd3;
}
