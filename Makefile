
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

.PHONY: all clean install uninstall tests
