{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs }:
let
  drv = import ./. { inherit reflex-platform; };
in
  if pkgs.lib.inNixShell then drv.env else drv
