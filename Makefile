.PHONY: test

build:
	dune build src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean: 
	dune clean

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	