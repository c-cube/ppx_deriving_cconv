
OCAMLBUILD=ocamlbuild -use-ocamlfind
OPTIONS=-package cconv

all:
	$(OCAMLBUILD) $(OPTIONS) src/ppx_deriving_cconv.native

clean:
	$(OCAMLBUILD) $(OPTIONS) -clean

.PHONY: all clean
