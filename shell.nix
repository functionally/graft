with import <nixpkgs> {};

let

in

  stdenv.mkDerivation rec {
    name = "ihaskell-local";
    buildInputs = [
      python3Packages.jupyter
      haskellPackages.ihaskell
      haskellPackages.unordered-containers
    ];
    buildPhase = ''
      echo $out
    '';
    shellHook = ''
      export HOME=$PWD
      ${haskellPackages.ihaskell}/bin/ihaskell install
      jupyter notebook --no-browser
      exit
    '';
  }
