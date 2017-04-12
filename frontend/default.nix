{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  cabal2nixResult = reflex-platform.cabal2nixResult;
  ghcjs = reflex-platform.ghcjs;
  ghcjsPackages = reflex-platform.ghcjsPackages.override {
    overrides = self: super: {
      servant-reflex = cabal2nixResult ../servant-reflex;
    };
  };
  drv = ghcjsPackages.callPackage ./frontend.nix {
    common = common { compiler = ghcjs; };
  };
in
if pkgs.lib.inNixShell then drv.env else drv

