{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "optional-args";
  version = "1.0.1";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Optional function arguments";
  license = stdenv.lib.licenses.bsd3;
}
