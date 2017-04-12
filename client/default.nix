{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  ghc = reflex-platform.ghc;
  drv = ghc.callPackage ./client.nix {
    common = common { compiler = ghc; };
  };
in
if pkgs.lib.inNixShell then drv.env else drv
