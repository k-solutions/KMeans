{ mkDerivation, base, container, hpack, mtl, random-fu, rvar
, stdenv
}:
mkDerivation {
  pname = "parBase";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base container mtl random-fu rvar ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base container mtl random-fu rvar ];
  testHaskellDepends = [ base container mtl random-fu rvar ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/parBase#readme";
  license = stdenv.lib.licenses.bsd3;
}
