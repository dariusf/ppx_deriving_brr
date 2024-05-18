
export OCAMLRUNPARAM=b

.PHONY: all
all:
	dune test
	dune build example/example.bc.js --display=short
	node _build/default/example/example.bc.js