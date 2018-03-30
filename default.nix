{ mkDerivation, base, containers, heaps, raft, stringbuilder, stdenv }:
mkDerivation {
  pname = "graft";
  version = "0.1.2.3";
  src = ./.;
  libraryHaskellDepends = [ base containers heaps raft stringbuilder ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
