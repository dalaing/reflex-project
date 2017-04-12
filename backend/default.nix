{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  ghc = reflex-platform.ghc;
  doJailbreak = reflex-platform.lib.doJailbreak;
  modified-ghc = ghc.override {
    overrides = self: super: {
      heist = doJailbreak super.heist;
      xmlhtml = doJailbreak super.xmlhtml;
      hspec-snap = doJailbreak super.hspec-snap;
    };
  };
  drv = modified-ghc.callPackage ./backend.nix {
    mkDerivation = ghc.mkDerivation;
    common = common { compiler = modified-ghc; };
  };
in
if pkgs.lib.inNixShell then drv.env else drv

