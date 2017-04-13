{ mkDerivation, base, common, containers, errors, ghcjs-dom, reflex
, reflex-dom, servant, servant-reflex, stdenv, text
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base common containers errors ghcjs-dom reflex reflex-dom servant
    servant-reflex text
  ];
  license = stdenv.lib.licenses.bsd3;
}
