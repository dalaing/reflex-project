{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs }:
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
  drv = modified-ghc.callPackage ./servant-server-reflex.nix {
    mkDerivation = ghc.mkDerivation;
  };
in
if pkgs.lib.inNixShell then drv.env else drv

