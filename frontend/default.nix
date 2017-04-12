{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  doJailbreak = reflex-platform.lib.doJailbreak;
  cabal2nixResult = reflex-platform.cabal2nixResult;
  ghcjs = reflex-platform.ghcjs;
  ghcjsPackages = reflex-platform.ghcjsPackages.override {
    overrides = self: super: {
      servant-reflex = doJailbreak (cabal2nixResult ../servant-reflex);
      dependent-sum = super.dependent-sum_0_4;
      dependent-sum-template = doJailbreak super.dependent-sum-template;
    };
  };
  drv = ghcjsPackages.callPackage ./frontend.nix {
    common = common { compiler = ghcjs; };
  };
in
if pkgs.lib.inNixShell then drv.env else drv

