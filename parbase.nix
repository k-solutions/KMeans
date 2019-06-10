let
  pkgs = import <nixpkgs> { };
in
  {  parbase = pkgs.haskellPackages.callPackage ./default.nix { };
  }
