{ pkgs ? (import <nixpkgs> {}) }:

let
  #inherit (pkgs) haskellPackages;
  haskellPackages = pkgs.haskellPackages_ghc782;
  c2hs = pkgs.haskellPackages_ghc763.c2hs; # Does not build yet with GHC 7.8
in
with haskellPackages;
cabal.mkDerivation (self: {
  pname = "alsa-mixer";
  version = "0.2.0";
  src = ./.;
  buildDepends = [ alsaCore ];
  buildTools = [ c2hs cabalInstall ];
  extraLibraries = [ pkgs.alsaLib ];
  meta = {
    description = "Bindings to the ALSA simple mixer API";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
