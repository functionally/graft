{ mkDerivation, base, containers, heaps, raft, stringbuilder, stdenv }:
mkDerivation {
  pname = "graft";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ base containers heaps raft stringbuilder ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
