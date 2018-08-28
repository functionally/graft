{ compiler ? "ghc7103" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              mkDerivation = args: haskellPackagesOld.mkDerivation ( args // {
                enableLibraryProfiling = true;
              # doCheck = false;
                doHaddock = false;
              });
              graft =
                haskellPackagesNew.callPackage ./default.nix { };
              heaps =
                haskellPackagesNew.callPackage ./heaps.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
   graft = pkgs.haskell.packages.${compiler}.graft;
  }
