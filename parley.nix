{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "parley";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/ajmccluskey/parley";
  description = "Light weight comments for the web";
  license = stdenv.lib.licenses.bsd3;
}
