{
  mkDerivation, base, containers, heaps, mtl, raft, stringbuilder, fgl, fgl-arbitrary, QuickCheck, stdenv,
  cabal-install, ghcid, hasktags, hdevtools, hlint, pointfree, pointful, threadscope
}:
mkDerivation {
  pname = "graft";
  version = "0.2.0.1";
  src = ./.;
  buildDepends = [ cabal-install ghcid hasktags hdevtools hlint pointfree pointful threadscope];
  libraryHaskellDepends = [ base containers heaps mtl raft stringbuilder fgl fgl-arbitrary QuickCheck];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
