
OCAMLBUILD=ocamlbuild -use-ocamlfind
OPTIONS=-package cconv

NAME=ppx_deriving_cconv
TARGETS=$(addprefix $(NAME), .cma .a .cmxa .cmxs)

all:
	$(OCAMLBUILD) $(OPTIONS) $(TARGETS)

clean:
	$(OCAMLBUILD) $(OPTIONS) -clean

install: all
	ocamlfind install $(NAME) META $(addprefix _build/src/,$(TARGETS))

uninstall:
	ocamlfind remove $(NAME)

build-tests: all
	$(OCAMLBUILD) $(OPTIONS) tests/run_tests.native

tests: build-tests
	./run_tests.native

# TODO fix
show-test-source:
	ocamlfind ocamlc -ppxopt ppx_deriving,_build/src/ppx_deriving_cconv.cma \
	    -package oUnit -package ppx_deriving.show -package cconv \
	    tests/run_tests.ml -dparsetree 2>&1 | less

.PHONY: all clean install uninstall tests
