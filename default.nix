{ pkgs ? (import <nixpkgs> {}) }:

let
  inherit (pkgs) haskellPackages;
in
with haskellPackages;
cabal.mkDerivation (self: {
  pname = "alsa-mixer";
  version = "0.2.0";
  src = ./.;
  buildDepends = [ alsaCore ];
  buildTools = [ c2hs ];
  extraLibraries = [ pkgs.alsaLib ];
  meta = {
    description = "Bindings to the ALSA simple mixer API";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
