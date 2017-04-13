{ reflex-platform ? import ./reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, ghc ? reflex-platform.ghc
, ghcjs ? reflex-platform.ghcjs
}:
let
  backend = ghc.callPackage ./backend { inherit reflex-platform; };
  frontend = ghcjs.callPackage ./frontend { inherit reflex-platform; };
in
  pkgs.writeScriptBin "serve-serial" ''
    #!${pkgs.stdenv.shell}
    ${backend}/bin/serve-serial ${frontend}/bin/frontend.jsexe/
  ''
