{
  mkDerivation, stdenv
, base, containers, heaps, monoid-absorbing, mtl, non-negative, raft, stringbuilder
, fgl, fgl-arbitrary, hmatrix-glpk, QuickCheck
, cabal-install, ghcid, hasktags, hdevtools, hlint, pointfree, pointful, threadscope
}:
mkDerivation {
  pname = "graft";
  version = "0.2.0.6";
  src = ./.;
  buildDepends = [
    cabal-install
    ghcid
    hasktags
    hdevtools
    hlint
    pointfree
    pointful
    threadscope
  ];
  libraryHaskellDepends = [
    base
    containers
    heaps
    monoid-absorbing
    mtl
    non-negative
    raft
    stringbuilder
  ];
  testHaskellDepends = [
    base
    containers
    heaps
    monoid-absorbing
    mtl
    non-negative
    raft
    stringbuilder
    fgl
    fgl-arbitrary
    hmatrix-glpk
    QuickCheck
  ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
