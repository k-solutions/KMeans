{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, container, deepseq
      , hpack, mtl, parallel, random, random-fu, rvar, stdenv, time
      , transformers, vector
      }:
      mkDerivation {
        pname = "kMeans";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base binary bytestring container deepseq mtl parallel random
          random-fu rvar time transformers vector
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base binary bytestring container deepseq mtl parallel random
          random-fu rvar time transformers vector
        ];
        testHaskellDepends = [
          base binary bytestring container deepseq mtl parallel random
          random-fu rvar time transformers vector
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/githubuser/parBase#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
