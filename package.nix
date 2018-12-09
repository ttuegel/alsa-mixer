{ mkDerivation, alsa-core, alsaLib, base, c2hs, stdenv, unix }:
mkDerivation {
  pname = "alsa-mixer";
  version = "0.2.0.3";
  src = ./.;
  libraryHaskellDepends = [ alsa-core base unix ];
  librarySystemDepends = [ alsaLib ];
  libraryToolDepends = [ c2hs ];
  homepage = "https://github.com/ttuegel/alsa-mixer";
  description = "Bindings to the ALSA simple mixer API";
  license = stdenv.lib.licenses.bsd3;
}
