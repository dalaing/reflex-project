{ mkDerivation, base, common, ghcjs-dom, reflex, reflex-dom
, servant-reflex, stdenv
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base common ghcjs-dom reflex reflex-dom servant-reflex
  ];
  license = stdenv.lib.licenses.bsd3;
}
