{ compiler }:

compiler.callPackage ./common.nix { mkDerivation = compiler.mkDerivation; }
