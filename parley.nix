{ mkDerivation, aeson, base, bytestring, http-types
, optparse-applicative, sqlite-simple, sqlite-simple-errors, stdenv
, text, time, wai, warp
}:
mkDerivation {
  pname = "parley";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-types optparse-applicative sqlite-simple
    sqlite-simple-errors text time wai warp
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/qfpl/parley";
  description = "Light weight comments for the web";
  license = stdenv.lib.licenses.bsd3;
}
