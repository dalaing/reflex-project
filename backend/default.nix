{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  ghc = reflex-platform.ghc;
  modified-ghc = ghc.override {
    overrides = self: super: {
      heist = pkgs.haskell.lib.doJailbreak super.heist;
      xmlhtml = pkgs.haskell.lib.doJailbreak super.xmlhtml;
      hspec-snap = pkgs.haskell.lib.doJailbreak super.hspec-snap;
    };
  };
  drv = modified-ghc.callPackage ./backend.nix {
    common = common { compiler = modified-ghc; };
  };
in
if pkgs.lib.inNixShell then drv.env else drv

