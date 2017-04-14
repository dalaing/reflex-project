{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  ghc = reflex-platform.ghc;
  drv = ghc.callPackage ./client.nix {
    mkDerivation = ghc.mkDerivation;
    common = common { compiler = ghc; };
  };
in
drv
